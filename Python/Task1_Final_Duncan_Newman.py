# Duncan Newman
# ID 01066373

# C:\Users\dunca\Downloads\city_weather.txt

# Loading the file

def load_weather_data(filename):
    weather_data = []
    with open(filename, 'r') as file:
        for line in file:
            parts = line.strip().split(',')
            if parts:
                city = parts[0]
                number = [int(x) for x in parts[1:]]
                weather_data.append({"city": city,"number":number})
    return weather_data

filename = "C:\\Users\\dunca\\OneDrive\\Desktop\\DASC 596\\city_weather.txt"

final_data = load_weather_data(filename)

print(final_data)




# Finding the Statistics
import statistics

def calculate_statistics(weather_data):
    
    # Loading the data as before and split into pieces
    with open(filename, 'r') as file:
        for line in file:
            parts = line.strip().split(',')
            if parts:
                city = parts[0]
                number = [int(x) for x in parts[1:]]

        #Finding the statistics we need
        
        average = statistics.mean(number)
        maximum = max(number)
        minimum = min(number)
    for item in city:
        print({city}, "average: ", {average})
        print({city}, "max: ", {maximum})
        print({city}, "min: ", {minimum})
    return 

filename = "C:\\Users\\dunca\\OneDrive\\Desktop\\DASC 596\\city_weather.txt"

stat_data = calculate_statistics(filename)



# Find the Hottest City

def hottest_city(weather_data):

#Start by starting with no hottest city to compare

    hottest_place = None

    hottest_temp = float('-inf') #Just in case of an infinity
    
    #Load the data as before
    with open(filename, 'r') as file:
        for line in file:
            parts = line.strip().split(',')
            if parts:
                city = parts[0]
                number = [int(x) for x in parts[1:]]
                
        # find the max temperature for the city
        maximum = max(number)
        
        if maximum > hottest_temp:
            hottest_temp = maximum
            hottest_place = city
            
    return hottest_place

filename = "C:\\Users\\dunca\\OneDrive\\Desktop\\DASC 596\\city_weather.txt"

hot_city = hottest_city(filename)

print("This hottest City is", hot_city)


# Find Cities above a threshold

def find_cities_above_threshold(weather_data):
    weather_data = []
    
    
    #Establishing a Threshold
    # Threshold = 20
    
    #Load the data as before
    with open(filename, 'r') as file:
        for line in file:
            parts = line.strip().split(',')
            if parts:
                city = parts[0]
                number = [int(x) for x in parts[1:]]
                weather_data.append({"city": city,"number":number})
        if 10 > 20:
            print(city)
        else:
            return city

cities = find_cities_above_threshold(filename)
        
print("Cities with temperatures above 20 degrees:", cities)
       
 
# Calculate the temperature range

def calculate_range(weather_data):
    
    # Loading the data from the .txt into parts again
    with open(filename, 'r') as file:
        for line in file:
            parts = line.strip().split(',')
            
            # Seperate into parts
            if parts:
                city = parts[0]
                number = [int(x) for x in parts[1:]]

        # Finding max, min and range for the function
        maximum = max(number)
        minimum = min(number)
        total_range = maximum - minimum
    for item in city:
        print({city}, "range", {total_range})
    return 

filename = "C:\\Users\\dunca\\OneDrive\\Desktop\\DASC 596\\city_weather.txt"

range_data = calculate_range(filename) 

print(range_data) 

       







    
    
