"""
FarmTech Solutions - Farm Management System

This script provides a menu-driven interface for managing crop data, including
input management, area calculations, and CSV import/export functionality.

Author: Gabriel Mule <gabemule@gmail.com>
Date: 2024
"""

import math
import csv
import json

# Load crop inputs from JSON file
with open('crop_inputs.json', 'r') as f:
    INPUTS = json.load(f)

# Define crop types based on INPUTS keys
CROP_TYPES = list(INPUTS.keys())

# Data storage
crop_data = []

def calculate_area(length, width):
    """
    Calculate rectangular area in hectares.
    
    Args:
    length (float): Length of the field in meters.
    width (float): Width of the field in meters.
    
    Returns:
    float: Area of the field in hectares.
    """
    return (length * width) / 10000  # Convert m² to hectares

def calculate_total_input(area, amount_per_ha):
    """
    Calculate total input needed for the crop.
    
    Args:
    area (float): Area of the field in hectares.
    amount_per_ha (float): Amount of input needed per hectare.
    
    Returns:
    float: Total amount of input needed for the given area.
    """
    return area * amount_per_ha

def input_crop_data():
    """
    Prompt user for crop data input and add it to the crop_data list.
    """
    print("\nSelect crop type:")
    for i, crop in enumerate(CROP_TYPES, 1):
        print(f"{i}. {crop}")
    
    while True:
        try:
            choice = int(input("Enter the number of your choice: "))
            if 1 <= choice <= len(CROP_TYPES):
                crop_type = CROP_TYPES[choice - 1]
                break
            else:
                print("Invalid choice. Please try again.")
        except ValueError:
            print("Please enter a valid number.")
    
    length = float(input("Enter field length (in meters): "))
    width = float(input("Enter field width (in meters): "))
    num_rows = int(input("Enter the number of rows in the field: "))
    
    area = calculate_area(length, width)
    
    inputs_data = {}
    available_inputs = INPUTS[crop_type]
    
    print(f"\nEnter the quantity for each input (or 0 to skip):")
    for input_type, input_info in available_inputs.items():
        while True:
            try:
                amount_per_ha = float(input(f"{input_type} ({input_info['name']}) in {input_info['unit']} per hectare: "))
                if amount_per_ha >= 0:
                    if amount_per_ha > 0:
                        total_amount = calculate_total_input(area, amount_per_ha)
                        inputs_data[input_type] = {
                            'name': input_info['name'],
                            'amount_per_ha': amount_per_ha,
                            'total_amount': total_amount,
                            'unit': input_info['unit']
                        }
                    break
                else:
                    print("Please enter a non-negative number.")
            except ValueError:
                print("Please enter a valid number.")
    
    crop_data.append({
        'type': crop_type,
        'length': length,
        'width': width,
        'area': area,
        'num_rows': num_rows,
        'inputs': inputs_data
    })
    print("Data added successfully.")

def display_crop_data():
    """
    Display all entered crop data.
    """
    if not crop_data:
        print("No data available.")
        return
    for idx, crop in enumerate(crop_data):
        print(f"\nCrop {idx + 1}:")
        print(f"Type: {crop['type']}")
        print(f"Field dimensions: {crop['length']}m x {crop['width']}m")
        print(f"Area: {crop['area']:.2f} ha")
        print(f"Number of rows: {crop['num_rows']}")
        print("Input Management:")
        for input_type, input_data in crop['inputs'].items():
            print(f"  {input_type} ({input_data['name']}):")
            print(f"    Amount per hectare: {input_data['amount_per_ha']:.2f} {input_data['unit']}")
            print(f"    Total amount needed: {input_data['total_amount']:.2f} {input_data['unit']}")

def update_crop_data():
    """
    Update an existing crop data entry.
    """
    if not crop_data:
        print("No data available to update.")
        return
    idx = int(input("Enter the index of the crop to update: ")) - 1
    if 0 <= idx < len(crop_data):
        input_crop_data()
        crop_data[idx] = crop_data.pop()
        print("Data updated successfully.")
    else:
        print("Invalid index.")

def delete_crop_data():
    """
    Delete an existing crop data entry.
    """
    if not crop_data:
        print("No data available to delete.")
        return
    idx = int(input("Enter the index of the crop to delete: ")) - 1
    if 0 <= idx < len(crop_data):
        del crop_data[idx]
        print("Data deleted successfully.")
    else:
        print("Invalid index.")

def export_to_csv():
    """
    Export all crop data to a CSV file.
    """
    if not crop_data:
        print("No data available to export.")
        return
    with open('crop_data.csv', 'w', newline='') as csvfile:
        fieldnames = ['type', 'length', 'width', 'area', 'num_rows']
        for crop in crop_data:
            for input_type in crop['inputs'].keys():
                for key in ['name', 'amount_per_ha', 'total_amount', 'unit']:
                    fieldnames.append(f"{input_type}_{key}")
        
        writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
        writer.writeheader()
        for crop in crop_data:
            row = {
                'type': crop['type'],
                'length': crop['length'],
                'width': crop['width'],
                'area': crop['area'],
                'num_rows': crop['num_rows']
            }
            for input_type, input_data in crop['inputs'].items():
                row[f"{input_type}_name"] = input_data['name']
                row[f"{input_type}_amount_per_ha"] = input_data['amount_per_ha']
                row[f"{input_type}_total_amount"] = input_data['total_amount']
                row[f"{input_type}_unit"] = input_data['unit']
            writer.writerow(row)
    print("Data exported to crop_data.csv successfully.")

def import_from_csv():
    """
    Import crop data from a CSV file.
    """
    filename = input("Enter the name of the CSV file to import: ")
    try:
        with open(filename, 'r') as csvfile:
            reader = csv.DictReader(csvfile)
            imported_data = []
            for row in reader:
                crop = {
                    'type': row['type'],
                    'length': float(row['length']),
                    'width': float(row['width']),
                    'area': float(row['area']),
                    'num_rows': int(row['num_rows']),
                    'inputs': {}
                }
                for input_type in INPUTS[row['type']].keys():
                    if f"{input_type}_name" in row:
                        crop['inputs'][input_type] = {
                            'name': row[f"{input_type}_name"],
                            'amount_per_ha': float(row[f"{input_type}_amount_per_ha"]),
                            'total_amount': float(row[f"{input_type}_total_amount"]),
                            'unit': row[f"{input_type}_unit"]
                        }
                imported_data.append(crop)
            
            global crop_data
            crop_data = imported_data
            print(f"Successfully imported {len(imported_data)} crops from {filename}.")
    except FileNotFoundError:
        print(f"File '{filename}' not found. Please make sure the file exists and try again.")
    except KeyError as e:
        print(f"Error: Missing column in CSV file. {str(e)}")
    except ValueError as e:
        print(f"Error: Invalid data in CSV file. {str(e)}")

def main_menu():
    """
    Display and handle the main menu options.
    """
    while True:
        print("\n--- FarmTech Solutions Menu ---")
        print("1. Enter crop data")
        print("2. Display crop data")
        print("3. Update crop data")
        print("4. Delete crop data")
        print("5. Export data to CSV")
        print("6. Import data from CSV")
        print("7. Exit")
        
        choice = input("Enter your choice (1-7): ")
        
        if choice == '1':
            input_crop_data()
        elif choice == '2':
            display_crop_data()
        elif choice == '3':
            update_crop_data()
        elif choice == '4':
            delete_crop_data()
        elif choice == '5':
            export_to_csv()
        elif choice == '6':
            import_from_csv()
        elif choice == '7':
            print("Exiting the program. Goodbye!")
            break
        else:
            print("Invalid choice. Please try again.")

if __name__ == "__main__":
    main_menu()