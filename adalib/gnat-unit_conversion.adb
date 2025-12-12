-- GNAT.Unit_Conversion body for Z80
-- Unit conversion implementation using integer arithmetic

package body GNAT.Unit_Conversion is

   -----------------
   -- Temperature --
   -----------------

   function Celsius_To_Fahrenheit (C_Tenths : Integer) return Integer is
   begin
      -- F = C * 9/5 + 32
      return (C_Tenths * 9) / 5 + 320;
   end Celsius_To_Fahrenheit;

   function Fahrenheit_To_Celsius (F_Tenths : Integer) return Integer is
   begin
      -- C = (F - 32) * 5/9
      return ((F_Tenths - 320) * 5) / 9;
   end Fahrenheit_To_Celsius;

   function Celsius_To_Kelvin (C_Tenths : Integer) return Integer is
   begin
      -- K = C + 273.15
      return C_Tenths + 2732;
   end Celsius_To_Kelvin;

   function Kelvin_To_Celsius (K_Tenths : Integer) return Integer is
   begin
      return K_Tenths - 2732;
   end Kelvin_To_Celsius;

   ------------
   -- Length --
   ------------

   function Inches_To_MM (Inches_Hundredths : Integer) return Integer is
   begin
      -- 1 inch = 25.4 mm
      return (Inches_Hundredths * 254) / 100;
   end Inches_To_MM;

   function MM_To_Inches (MM : Integer) return Integer is
   begin
      return (MM * 100) / 254;
   end MM_To_Inches;

   function Feet_To_CM (Feet_Hundredths : Integer) return Integer is
   begin
      -- 1 foot = 30.48 cm
      return (Feet_Hundredths * 3048) / 100;
   end Feet_To_CM;

   function CM_To_Feet (CM : Integer) return Integer is
   begin
      return (CM * 100) / 3048;
   end CM_To_Feet;

   function Miles_To_KM (Miles_Hundredths : Integer) return Integer is
   begin
      -- 1 mile = 1.609 km
      return (Miles_Hundredths * 1609) / 1000;
   end Miles_To_KM;

   function KM_To_Miles (KM_Hundredths : Integer) return Integer is
   begin
      return (KM_Hundredths * 1000) / 1609;
   end KM_To_Miles;

   function Yards_To_Meters (Yards : Integer) return Integer is
   begin
      -- 1 yard = 0.9144 m = 91.44 cm
      return (Yards * 9144) / 100;
   end Yards_To_Meters;

   function Meters_To_Yards (Meters_CM : Integer) return Integer is
   begin
      return (Meters_CM * 100) / 9144;
   end Meters_To_Yards;

   ------------
   -- Weight --
   ------------

   function Pounds_To_Grams (Pounds_Hundredths : Integer) return Integer is
   begin
      -- 1 pound = 453.592 grams
      return (Pounds_Hundredths * 4536) / 100;
   end Pounds_To_Grams;

   function Grams_To_Pounds (Grams : Integer) return Integer is
   begin
      return (Grams * 100) / 4536;
   end Grams_To_Pounds;

   function Ounces_To_Grams (Ounces_Tenths : Integer) return Integer is
   begin
      -- 1 ounce = 28.35 grams
      return (Ounces_Tenths * 2835) / 100;
   end Ounces_To_Grams;

   function Grams_To_Ounces (Grams : Integer) return Integer is
   begin
      return (Grams * 100) / 2835;
   end Grams_To_Ounces;

   function KG_To_Pounds (KG_Hundredths : Integer) return Integer is
   begin
      -- 1 kg = 2.205 pounds
      return (KG_Hundredths * 2205) / 1000;
   end KG_To_Pounds;

   function Pounds_To_KG (Pounds_Hundredths : Integer) return Integer is
   begin
      return (Pounds_Hundredths * 1000) / 2205;
   end Pounds_To_KG;

   ------------
   -- Volume --
   ------------

   function Gallons_To_Liters (Gallons_Hundredths : Integer) return Integer is
   begin
      -- 1 gallon = 3.785 liters = 3785 ml
      return (Gallons_Hundredths * 3785) / 100;
   end Gallons_To_Liters;

   function Liters_To_Gallons (ML : Integer) return Integer is
   begin
      return (ML * 100) / 3785;
   end Liters_To_Gallons;

   function Pints_To_ML (Pints_Tenths : Integer) return Integer is
   begin
      -- 1 pint = 473 ml
      return (Pints_Tenths * 473) / 10;
   end Pints_To_ML;

   function ML_To_Pints (ML : Integer) return Integer is
   begin
      return (ML * 10) / 473;
   end ML_To_Pints;

   function Fluid_Oz_To_ML (Oz_Tenths : Integer) return Integer is
   begin
      -- 1 fl oz = 29.57 ml
      return (Oz_Tenths * 2957) / 100;
   end Fluid_Oz_To_ML;

   function ML_To_Fluid_Oz (ML : Integer) return Integer is
   begin
      return (ML * 100) / 2957;
   end ML_To_Fluid_Oz;

   ----------
   -- Area --
   ----------

   function Sq_Inches_To_Sq_CM (Sq_Inches : Integer) return Integer is
   begin
      -- 1 sq inch = 6.452 sq cm
      return (Sq_Inches * 6452) / 1000;
   end Sq_Inches_To_Sq_CM;

   function Sq_CM_To_Sq_Inches (Sq_CM : Integer) return Integer is
   begin
      return (Sq_CM * 1000) / 6452;
   end Sq_CM_To_Sq_Inches;

   function Sq_Feet_To_Sq_Meters (Sq_Feet : Integer) return Integer is
   begin
      -- 1 sq foot = 0.0929 sq meters
      return (Sq_Feet * 929) / 100;
   end Sq_Feet_To_Sq_Meters;

   function Sq_Meters_To_Sq_Feet (Sq_Meters_Hundredths : Integer) return Integer is
   begin
      return (Sq_Meters_Hundredths * 100) / 929;
   end Sq_Meters_To_Sq_Feet;

   function Acres_To_Hectares (Acres_Hundredths : Integer) return Integer is
   begin
      -- 1 acre = 0.4047 hectares
      return (Acres_Hundredths * 4047) / 10000;
   end Acres_To_Hectares;

   function Hectares_To_Acres (Hectares_Hundredths : Integer) return Integer is
   begin
      return (Hectares_Hundredths * 10000) / 4047;
   end Hectares_To_Acres;

   -----------
   -- Speed --
   -----------

   function MPH_To_KPH (MPH_Tenths : Integer) return Integer is
   begin
      -- 1 mph = 1.609 kph
      return (MPH_Tenths * 1609) / 1000;
   end MPH_To_KPH;

   function KPH_To_MPH (KPH_Tenths : Integer) return Integer is
   begin
      return (KPH_Tenths * 1000) / 1609;
   end KPH_To_MPH;

   function Knots_To_KPH (Knots_Tenths : Integer) return Integer is
   begin
      -- 1 knot = 1.852 kph
      return (Knots_Tenths * 1852) / 1000;
   end Knots_To_KPH;

   function KPH_To_Knots (KPH_Tenths : Integer) return Integer is
   begin
      return (KPH_Tenths * 1000) / 1852;
   end KPH_To_Knots;

   ----------
   -- Time --
   ----------

   function Hours_To_Minutes (Hours : Integer) return Integer is
   begin
      return Hours * 60;
   end Hours_To_Minutes;

   function Minutes_To_Hours (Minutes : Integer) return Integer is
   begin
      return Minutes / 60;
   end Minutes_To_Hours;

   function Hours_To_Seconds (Hours : Integer) return Integer is
   begin
      return Hours * 3600;
   end Hours_To_Seconds;

   function Seconds_To_Hours (Seconds : Integer) return Integer is
   begin
      return Seconds / 3600;
   end Seconds_To_Hours;

   function Days_To_Hours (Days : Integer) return Integer is
   begin
      return Days * 24;
   end Days_To_Hours;

   function Hours_To_Days (Hours : Integer) return Integer is
   begin
      return Hours / 24;
   end Hours_To_Days;

   function Weeks_To_Days (Weeks : Integer) return Integer is
   begin
      return Weeks * 7;
   end Weeks_To_Days;

   function Days_To_Weeks (Days : Integer) return Integer is
   begin
      return Days / 7;
   end Days_To_Weeks;

   --------------
   -- Pressure --
   --------------

   function PSI_To_KPa (PSI_Tenths : Integer) return Integer is
   begin
      -- 1 PSI = 6.895 kPa
      return (PSI_Tenths * 6895) / 1000;
   end PSI_To_KPa;

   function KPa_To_PSI (KPa_Tenths : Integer) return Integer is
   begin
      return (KPa_Tenths * 1000) / 6895;
   end KPa_To_PSI;

   function Bar_To_PSI (Bar_Hundredths : Integer) return Integer is
   begin
      -- 1 bar = 14.504 PSI
      return (Bar_Hundredths * 14504) / 1000;
   end Bar_To_PSI;

   function PSI_To_Bar (PSI_Tenths : Integer) return Integer is
   begin
      return (PSI_Tenths * 1000) / 14504;
   end PSI_To_Bar;

   function ATM_To_KPa (ATM_Hundredths : Integer) return Integer is
   begin
      -- 1 atm = 101.325 kPa
      return (ATM_Hundredths * 10133) / 100;
   end ATM_To_KPa;

   function KPa_To_ATM (KPa_Tenths : Integer) return Integer is
   begin
      return (KPa_Tenths * 100) / 10133;
   end KPa_To_ATM;

   ------------
   -- Energy --
   ------------

   function Calories_To_Joules (Cal : Integer) return Integer is
   begin
      -- 1 calorie = 4.184 joules
      return (Cal * 4184) / 1000;
   end Calories_To_Joules;

   function Joules_To_Calories (J : Integer) return Integer is
   begin
      return (J * 1000) / 4184;
   end Joules_To_Calories;

   function KWh_To_Joules (KWh_Hundredths : Integer) return Integer is
   begin
      -- 1 kWh = 3600 kJ
      return (KWh_Hundredths * 36);  -- Returns kJ
   end KWh_To_Joules;

   function BTU_To_Joules (BTU : Integer) return Integer is
   begin
      -- 1 BTU = 1055.06 joules
      return (BTU * 1055);
   end BTU_To_Joules;

   -----------
   -- Angle --
   -----------

   function Degrees_To_Radians (Deg_Hundredths : Integer) return Integer is
   begin
      -- radians = degrees * pi / 180
      -- pi ~= 3.14159, so pi/180 ~= 0.01745
      return (Deg_Hundredths * 1745) / 10000;
   end Degrees_To_Radians;

   function Radians_To_Degrees (Rad_Thousandths : Integer) return Integer is
   begin
      -- degrees = radians * 180 / pi
      -- 180/pi ~= 57.2958
      return (Rad_Thousandths * 5730) / 100;
   end Radians_To_Degrees;

   ------------------
   -- Data Storage --
   ------------------

   function Bytes_To_KB (Bytes : Integer) return Integer is
   begin
      return Bytes / 1024;
   end Bytes_To_KB;

   function KB_To_Bytes (KB : Integer) return Integer is
   begin
      return KB * 1024;
   end KB_To_Bytes;

   function KB_To_MB (KB : Integer) return Integer is
   begin
      return KB / 1024;
   end KB_To_MB;

   function MB_To_KB (MB : Integer) return Integer is
   begin
      return MB * 1024;
   end MB_To_KB;

   function Bits_To_Bytes (Bits : Integer) return Integer is
   begin
      return Bits / 8;
   end Bits_To_Bytes;

   function Bytes_To_Bits (Bytes : Integer) return Integer is
   begin
      return Bytes * 8;
   end Bytes_To_Bits;

end GNAT.Unit_Conversion;
