-- GNAT.Unit_Conversion for Z80
-- Common unit conversions using integer arithmetic with scaling

package GNAT.Unit_Conversion is
   pragma Pure;

   -- All functions use scaled integers for precision
   -- Results are often scaled by 100 or 1000 to maintain precision

   -----------------
   -- Temperature --
   -----------------

   -- Input/output in tenths of degrees (e.g., 212 = 21.2 degrees)
   function Celsius_To_Fahrenheit (C_Tenths : Integer) return Integer;
   function Fahrenheit_To_Celsius (F_Tenths : Integer) return Integer;
   function Celsius_To_Kelvin (C_Tenths : Integer) return Integer;
   function Kelvin_To_Celsius (K_Tenths : Integer) return Integer;

   ------------
   -- Length --
   ------------

   -- Millimeters as base unit
   function Inches_To_MM (Inches_Hundredths : Integer) return Integer;
   function MM_To_Inches (MM : Integer) return Integer;  -- Returns hundredths
   function Feet_To_CM (Feet_Hundredths : Integer) return Integer;
   function CM_To_Feet (CM : Integer) return Integer;  -- Returns hundredths
   function Miles_To_KM (Miles_Hundredths : Integer) return Integer;
   function KM_To_Miles (KM_Hundredths : Integer) return Integer;
   function Yards_To_Meters (Yards : Integer) return Integer;  -- Returns cm
   function Meters_To_Yards (Meters_CM : Integer) return Integer;

   ------------
   -- Weight --
   ------------

   -- Grams as base unit
   function Pounds_To_Grams (Pounds_Hundredths : Integer) return Integer;
   function Grams_To_Pounds (Grams : Integer) return Integer;  -- Returns hundredths
   function Ounces_To_Grams (Ounces_Tenths : Integer) return Integer;
   function Grams_To_Ounces (Grams : Integer) return Integer;  -- Returns tenths
   function KG_To_Pounds (KG_Hundredths : Integer) return Integer;
   function Pounds_To_KG (Pounds_Hundredths : Integer) return Integer;

   ------------
   -- Volume --
   ------------

   -- Milliliters as base unit
   function Gallons_To_Liters (Gallons_Hundredths : Integer) return Integer;  -- Returns ML
   function Liters_To_Gallons (ML : Integer) return Integer;  -- Returns hundredths
   function Pints_To_ML (Pints_Tenths : Integer) return Integer;
   function ML_To_Pints (ML : Integer) return Integer;  -- Returns tenths
   function Fluid_Oz_To_ML (Oz_Tenths : Integer) return Integer;
   function ML_To_Fluid_Oz (ML : Integer) return Integer;  -- Returns tenths

   ----------
   -- Area --
   ----------

   -- Square centimeters as base unit
   function Sq_Inches_To_Sq_CM (Sq_Inches : Integer) return Integer;
   function Sq_CM_To_Sq_Inches (Sq_CM : Integer) return Integer;
   function Sq_Feet_To_Sq_Meters (Sq_Feet : Integer) return Integer;  -- Returns hundredths
   function Sq_Meters_To_Sq_Feet (Sq_Meters_Hundredths : Integer) return Integer;
   function Acres_To_Hectares (Acres_Hundredths : Integer) return Integer;
   function Hectares_To_Acres (Hectares_Hundredths : Integer) return Integer;

   -----------
   -- Speed --
   -----------

   -- Meters per hour as base unit (for Z80 integer math)
   function MPH_To_KPH (MPH_Tenths : Integer) return Integer;  -- Returns tenths
   function KPH_To_MPH (KPH_Tenths : Integer) return Integer;  -- Returns tenths
   function Knots_To_KPH (Knots_Tenths : Integer) return Integer;
   function KPH_To_Knots (KPH_Tenths : Integer) return Integer;

   ----------
   -- Time --
   ----------

   function Hours_To_Minutes (Hours : Integer) return Integer;
   function Minutes_To_Hours (Minutes : Integer) return Integer;  -- Truncated
   function Hours_To_Seconds (Hours : Integer) return Integer;
   function Seconds_To_Hours (Seconds : Integer) return Integer;  -- Truncated
   function Days_To_Hours (Days : Integer) return Integer;
   function Hours_To_Days (Hours : Integer) return Integer;  -- Truncated
   function Weeks_To_Days (Weeks : Integer) return Integer;
   function Days_To_Weeks (Days : Integer) return Integer;  -- Truncated

   --------------
   -- Pressure --
   --------------

   -- Pascals as base unit (scaled for Z80)
   function PSI_To_KPa (PSI_Tenths : Integer) return Integer;  -- Returns tenths
   function KPa_To_PSI (KPa_Tenths : Integer) return Integer;  -- Returns tenths
   function Bar_To_PSI (Bar_Hundredths : Integer) return Integer;  -- Returns tenths
   function PSI_To_Bar (PSI_Tenths : Integer) return Integer;  -- Returns hundredths
   function ATM_To_KPa (ATM_Hundredths : Integer) return Integer;  -- Returns tenths
   function KPa_To_ATM (KPa_Tenths : Integer) return Integer;  -- Returns hundredths

   ------------
   -- Energy --
   ------------

   function Calories_To_Joules (Cal : Integer) return Integer;
   function Joules_To_Calories (J : Integer) return Integer;
   function KWh_To_Joules (KWh_Hundredths : Integer) return Integer;  -- Returns KJ
   function BTU_To_Joules (BTU : Integer) return Integer;

   -----------
   -- Angle --
   -----------

   -- Using scaled values: degrees * 100, radians * 1000
   function Degrees_To_Radians (Deg_Hundredths : Integer) return Integer;
   function Radians_To_Degrees (Rad_Thousandths : Integer) return Integer;

   ------------------
   -- Data Storage --
   ------------------

   function Bytes_To_KB (Bytes : Integer) return Integer;
   function KB_To_Bytes (KB : Integer) return Integer;
   function KB_To_MB (KB : Integer) return Integer;
   function MB_To_KB (MB : Integer) return Integer;
   function Bits_To_Bytes (Bits : Integer) return Integer;
   function Bytes_To_Bits (Bytes : Integer) return Integer;

end GNAT.Unit_Conversion;
