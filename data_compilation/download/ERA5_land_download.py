import cdsapi
import requests

c = cdsapi.Client(full_stack=True)

# Accumulations (ie variables accumulated from the beginning of the forecast to the end of the forecast step, see ERA5-Land documentation)
acc_vars = ['total_precipitation', 'surface_solar_radiation_downwards', 'potential_evaporation']

# Retrieve all months for a given year
months = ['01', '02', '03',
            '04', '05', '06',
            '07', '08', '09',
            '10', '11', '12']
            
# Retrieve all days for a given month
days = ['01','02','03', '04','05','06',
		'07','08','09', '10','11','12',
		'13','14','15', '16','17','18',
		'19','20','21', '22','23','24',
		'25','26','27', '28','29','30',
		'31']
        





def era5_land(years, var):
    print(var)
    # check if the variable is accumulated    
    if var in acc_vars:     
        print("Accumulated variable, script will download only the end of the forecast step (00:00)")
        hours = ['00:00']
    else:
        hours = ['00:00','01:00','02:00', '03:00','04:00','05:00',
	     '06:00','07:00','08:00', '09:00','10:00','11:00',
	     '12:00','13:00','14:00', '15:00','16:00','17:00',
	     '18:00','19:00','20:00', '21:00','22:00','23:00']

    # Loop over years and months
    for yr in years:
        for mn in months:
            r = c.service("catalogue.retrieve",
            'reanalysis-era5-land',
            {"product_type": "reanalysis", 
            "variable": var,
            "year": yr,
            "month": mn,
            "day": days,
            'time': hours,
            "area" : str(72)+'/'+str(-14)+'/'+str(34)+'/'+str(40)})
            
            file_name = var + "_" + yr + "_" + mn + ".nc"
            
            r.download(file_name)
        
        
            