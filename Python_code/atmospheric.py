"""
atmospheric.py, Sam Murphy (2016-10-26)
Atmospheric water vapour, ozone and AOT from GEE
Usage
H2O = Atmospheric.water(geom,date)
O3 = Atmospheric.ozone(geom,date)
AOT = Atmospheric.aerosol(geom,date)

class Conversion(), By Cristian Iranzo & Diego Alarcón
Iterative process
Usage
Conversion.conversion(geom, ImageCollection, GEE path to save corrected ImageCollection)
"""

import ee
import geemap
from Py6S import *
import os, sys, time, math, datetime

class Atmospheric():

  def round_date(date,xhour):
    """
    rounds a date of to the closest 'x' hours
    """
    y = date.get('year')
    m = date.get('month')
    d = date.get('day')
    H = date.get('hour')
    HH = H.divide(xhour).round().multiply(xhour)
    return date.fromYMD(y,m,d).advance(HH,'hour')
  
  def round_month(date):
    """
    round date to closest month
    """
    # start of THIS month
    m1 = date.fromYMD(date.get('year'),date.get('month'),ee.Number(1))
    
    # start of NEXT month
    m2 = m1.advance(1,'month')
      
    # difference from date
    d1 = ee.Number(date.difference(m1,'day')).abs()
    d2 = ee.Number(date.difference(m2,'day')).abs()
    
    # return closest start of month
    return ee.Date(ee.Algorithms.If(d2.gt(d1),m1,m2))
  
  
  
  def water(geom,date):
    """
    Water vapour column above target at time of image aquisition.
    
    (Kalnay et al., 1996, The NCEP/NCAR 40-Year Reanalysis Project. Bull. 
    Amer. Meteor. Soc., 77, 437-471)
    """
    
    # Point geometry required
    centroid = geom.centroid()
    
    # H2O datetime is in 6 hour intervals
    H2O_date = Atmospheric.round_date(date,6)
    
    # filtered water collection
    water_ic = ee.ImageCollection('NCEP_RE/surface_wv').filterDate(H2O_date, H2O_date.advance(1,'month'))
    
    # water image
    water_img = ee.Image(water_ic.first())
    
    # water_vapour at target
    water = water_img.reduceRegion(reducer=ee.Reducer.mean(), geometry=centroid).get('pr_wtr')
                                        
    # convert to Py6S units (Google = kg/m^2, Py6S = g/cm^2)
    water_Py6S_units = ee.Number(water).divide(10)                                   
    
    return water_Py6S_units
  
  
  
  def ozone(geom,date):
    """
    returns ozone measurement from merged TOMS/OMI dataset
    
    OR
    
    uses our fill value (which is mean value for that latlon and day-of-year)
  
    """
    
    # Point geometry required
    centroid = geom.centroid()
       
    def ozone_measurement(centroid,O3_date):
      
      # filtered ozone collection
      ozone_ic = ee.ImageCollection('TOMS/MERGED').filterDate(O3_date, O3_date.advance(1,'month'))
      
      # ozone image
      ozone_img = ee.Image(ozone_ic.first())
      
      # ozone value IF TOMS/OMI image exists ELSE use fill value
      ozone = ee.Algorithms.If(ozone_img,\
      ozone_img.reduceRegion(reducer=ee.Reducer.mean(), geometry=centroid).get('ozone'),\
      ozone_fill(centroid,O3_date))
      
      return ozone
      
    def ozone_fill(centroid,O3_date):
      """
      Gets our ozone fill value (i.e. mean value for that doy and latlon)
      
      you can see it
      1) compared to LEDAPS: https://code.earthengine.google.com/8e62a5a66e4920e701813e43c0ecb83e
      2) as a video: https://www.youtube.com/watch?v=rgqwvMRVguI&feature=youtu.be
      
      """
      
      # ozone fills (i.e. one band per doy)
      ozone_fills = ee.ImageCollection('users/samsammurphy/public/ozone_fill').toList(366)
      
      # day of year index
      jan01 = ee.Date.fromYMD(O3_date.get('year'),1,1)
      doy_index = date.difference(jan01,'day').toInt()# (NB. index is one less than doy, so no need to +1)
      
      # day of year image
      fill_image = ee.Image(ozone_fills.get(doy_index))
      
      # return scalar fill value
      return fill_image.reduceRegion(reducer=ee.Reducer.mean(), geometry=centroid).get('ozone')
     
    # O3 datetime in 24 hour intervals
    O3_date = Atmospheric.round_date(date,24)
    
    # TOMS temporal gap
    TOMS_gap = ee.DateRange('1994-11-01','1996-08-01')  
    
    # avoid TOMS gap entirely
    ozone = ee.Algorithms.If(TOMS_gap.contains(O3_date),ozone_fill(centroid,O3_date),ozone_measurement(centroid,O3_date))
    
    # fix other data gaps (e.g. spatial, missing images, etc..)
    ozone = ee.Algorithms.If(ozone,ozone,ozone_fill(centroid,O3_date))
    
    #convert to Py6S units 
    ozone_Py6S_units = ee.Number(ozone).divide(1000)# (i.e. Dobson units are milli-atm-cm )                             
    
    return ozone_Py6S_units
 

  def aerosol(geom,date):
    """
    Aerosol Optical Thickness.
    
    try:
      MODIS Aerosol Product (monthly)
    except:
      fill value
    """
    
    def aerosol_fill(date):
      """
      MODIS AOT fill value for this month (i.e. no data gaps)
      """
      return ee.Image('users/samsammurphy/public/AOT_stack')\
               .select([ee.String('AOT_').cat(date.format('M'))])\
               .rename(['AOT_550'])
               
               
    def aerosol_this_month(date):
      """
      MODIS AOT original data product for this month (i.e. some data gaps)
      """
      # image for this month
      img =  ee.Image(\
                      ee.ImageCollection('MODIS/006/MOD08_M3')\
                        .filterDate(Atmospheric.round_month(date))\
                        .first()\
                     )
      
      # fill missing month (?)
      img = ee.Algorithms.If(img,\
                               # all good
                               img\
                               .select(['Aerosol_Optical_Depth_Land_Mean_Mean_550'])\
                               .divide(1000)\
                               .rename(['AOT_550']),\
                              # missing month
                                aerosol_fill(date))
                      
      return img    
        
  
    def get_AOT(AOT_band,geom):
      """
      AOT scalar value for target
      """  
      return ee.Image(AOT_band).reduceRegion(reducer=ee.Reducer.mean(),\
                                 geometry=geom.centroid())\
                                .get('AOT_550')
                                

    after_modis_start = date.difference(ee.Date('2000-03-01'),'month').gt(0)
    
    AOT_band = ee.Algorithms.If(after_modis_start, aerosol_this_month(date), aerosol_fill(date))
    
    AOT = get_AOT(AOT_band,geom)
    
    AOT = ee.Algorithms.If(AOT,AOT,get_AOT(aerosol_fill(date),geom))
    # i.e. check reduce region worked (else force fill value)
    
    return AOT

class Conversion():
            
    # Main function
    # Reference idea from Cristian Iranzo https://github.com/samsammurphy/gee-atmcorr-S2/issues/7
    def conversion(geom, imgCol, gpath):
        
        region = geom.buffer(1000).bounds().getInfo()['coordinates']
        
        # Spectral Response functions
        def spectralResponseFunction(bandname):
            """
            Extract spectral response function for given band name
            """
            bandSelect = {
                'B1':PredefinedWavelengths.S2A_MSI_01,
                'B2':PredefinedWavelengths.S2A_MSI_02,
                'B3':PredefinedWavelengths.S2A_MSI_03,
                'B4':PredefinedWavelengths.S2A_MSI_04,
                'B5':PredefinedWavelengths.S2A_MSI_05,
                'B6':PredefinedWavelengths.S2A_MSI_06,
                'B7':PredefinedWavelengths.S2A_MSI_07,
                'B8':PredefinedWavelengths.S2A_MSI_08,
                'B8A':PredefinedWavelengths.S2A_MSI_8A,
                'B9':PredefinedWavelengths.S2A_MSI_09,
                'B10':PredefinedWavelengths.S2A_MSI_10,
                'B11':PredefinedWavelengths.S2A_MSI_11,
                'B12':PredefinedWavelengths.S2A_MSI_12,
                }

            return Wavelength(bandSelect[bandname])

        # TOA Reflectance to Radiance
        def toa_to_rad(bandname):
            """
            Converts top of atmosphere reflectance to at-sensor radiance
            """

            # solar exoatmospheric spectral irradiance
            ESUN = info['SOLAR_IRRADIANCE_'+bandname]
            solar_angle_correction = math.cos(math.radians(solar_z))

            # Earth-Sun distance (from day of year)
            doy = scene_date.timetuple().tm_yday
            d = 1 - 0.01672 * math.cos(0.9856 * (doy-4))
            # http://physics.stackexchange.com/
            # questions/177949/earth-sun-distance-on-a-given-day-of-the-year

            # conversion factor
            multiplier = ESUN*solar_angle_correction/(math.pi*d**2)

            # at-sensor radiance
            rad = toa.select(bandname).multiply(multiplier)

            return rad

        # Radiance to Surface Reflectance

        def surface_reflectance(bandname):
            """
            Calculate surface reflectance from at-sensor radiance given waveband name
            """

            # run 6S for this waveband
            s.wavelength = spectralResponseFunction(bandname)
            s.run()

            # extract 6S outputs
            Edir = s.outputs.direct_solar_irradiance             #direct solar irradiance
            Edif = s.outputs.diffuse_solar_irradiance            #diffuse solar irradiance
            Lp   = s.outputs.atmospheric_intrinsic_radiance      #path radiance
            absorb  = s.outputs.trans['global_gas'].upward       #absorption transmissivity
            scatter = s.outputs.trans['total_scattering'].upward #scattering transmissivity
            tau2 = absorb*scatter                                #total transmissivity

            # radiance to surface reflectance
            rad = toa_to_rad(bandname)
            ref = rad.subtract(Lp).multiply(math.pi).divide(tau2*(Edir+Edif))

            return ref
        
        # List with images to filter
        features = imgCol.getInfo()['features']

        for i in features:
            # Filter by ID in the list by image
            id = i['id']
            print("Bands: ")
            # Selection of the image
            img  = ee.Image(id)

            # write image date
            date = img.date()

            # Defining global variables:
            global toa
            global info
            global scene_date
            global solar_z
            global s

            # top of atmosphere reflectance
            toa = img.divide(10000)

            # METADATA
            info = img.getInfo()['properties']
            scene_date = datetime.datetime\
                .utcfromtimestamp(info['system:time_start']/1000)
            solar_z = info['MEAN_SOLAR_ZENITH_ANGLE']

            # ATMOSPHERIC CONSTITUENTS
            h2o = Atmospheric.water(geom,date).getInfo()
            o3 = Atmospheric.ozone(geom,date).getInfo()
            # Atmospheric Optical Thickness
            aot = Atmospheric.aerosol(geom,date).getInfo()

            # TARGET ALTITUDE (km)
            SRTM = ee.Image('CGIAR/SRTM90_V4')
            alt = SRTM.reduceRegion(reducer = ee.Reducer.mean(),
                geometry = geom.centroid()).get('elevation').getInfo()
            km = alt/1000 # i.e. Py6S uses units of kilometers

            # 6S OBJECT
            # Instantiate
            s = SixS()

            # Atmospheric constituents
            s.atmos_profile = AtmosProfile.UserWaterAndOzone(h2o,o3)
            s.aero_profile = AeroProfile.Continental
            s.aot550 = aot

            # Earth-Sun-satellite geometry
            s.geometry = Geometry.User()
            s.geometry.view_z = 0               # calculation assuming vision in NADIR
            s.geometry.solar_z = solar_z        # solar zenith angle
            s.geometry.month = scene_date.month # month used in distance Earth-Sun
            s.geometry.day = scene_date.day     # day used in the distance Earth-Sun
            s.altitudes\
                .set_sensor_satellite_level()   # Sensor altitude
            s.altitudes\
                .set_target_custom_altitude(km) # Altitude of the surface

            # ATMOSPHERIC CORRECTION (by waveband)
            output = img.select('QA60')
            for band in ['B1','B2','B3','B4','B5','B6','B7','B8','B8A','B9','B10','B11','B12']:
                     print(band, end=' ')
                     output = output.addBands(surface_reflectance(band))

            # set some properties for export
            dateString = scene_date.strftime("%Y-%m-%d")
            ref = output.set({'satellite':'Sentinel 2',
                           'fileID':info['system:index'],
                           'date':dateString,
                           'aerosol_optical_thickness':aot,
                           'water_vapour':h2o,
                           'ozone':o3})

            # define YOUR assetID or folder
            assetID = gpath + dateString

            # export
            export = ee.batch.Export.image.toAsset(\
                 image = ref,
                 description = 'sentinel2_atmcorr_export',
                 assetId = assetID,
                 region = region,
                 crs = 'EPSG:4326',
                 scale = 10)


            export.start()
            # print a message for each exported image
            print("image "+ assetID +" exported")
            print('\n')
            time.sleep(1)

        return print("Conversion ready")