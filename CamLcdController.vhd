-- Author	: Usman Ali Butt	
-- Date		: 28 september 2023

-- Cam main clock is 12Mhz supplied from FPGA pin.
-- Cam pixel clock is 6Mhz. Divided the pclk/2. ov7670
-- auto adjusts the frame size when pclk is reduced.
-- In default settings pclk = mclk.
-- In pclk/2 case frame is (640x480)/2. In single frame of 640x480
-- two images of 320x240 are side by side.

-- lcd and ov7670 modules are instantiated here. lcd configs and 
-- displays data - video frame. Lcd in 3.5 inch with active pixels 
-- array of 480x320. Ov7670 module only configs the Cam. video data frames
-- are picked here by controller and passed to lcd for display.

-- Controller process is pclk sensitive. Lcd is clk sensitive.
-- When lcd and ov7670 modules are done with their configurations. Video strobe or
-- wr signal control is taken over by controller module and on each positive edge 
-- of pclk it grabes 8-bit cam pixel data and sends to lcd for display.

-- Lcd strobe requires 50-66ns according to ILI9486 data sheet. 
-- Our pclk clock is 6Mhz or 166.66ns. Enough to read pixel data 
-- from cam and display on lcd.
-- Default pclk of 12Mhz does not work. Though 12Mhz translates to 
-- 83ns which is greater than lcd strobe requirement. Our FPGA is 
-- also working at same frequency. Increasing FPGA main clock to 24Mhz
-- will make pclk of 12Mhz to work with ILI9486.      

-- Our frame rate theoratically now equals = (pclk - 1) / (frame sixe x pixel size) 
-- frame size = 480 x 640
-- pixel size = 2 bytes
-- pclk = 6Mhz
-- I am perfectly receiving 
-- I noticed a blanking of 1 or 2 pixels at the sides of image. Dont know what it is for.
-- We can negate blanking from frame rate. Wish I could find ov7670 proper datasheet. 


library ieee;
use ieee.std_logic_1164.all;

entity CamLcdController is 
	port(
		clk			: in 	std_logic; 						-- Main clock 12Mhz
		pclk		: in 	std_logic; 						-- Pixel clock from camera
		Mclk		: out 	std_logic; 						-- Camera input clock
		cam_data  	: in 	std_logic_vector(7 downto 0);	-- Camera data 8-bit parallel
		lcd_data  	: out 	std_logic_vector(7 downto 0);	-- Lcd data 8-bit parallel
		cam_scl		: inout std_logic; 						-- Camera sccb scl pin
		cam_sda		: inout std_logic; 						-- Camera sccb sda pin
		leds		: out 	std_logic_vector(4 downto 0);	-- FPGA status leds - random use here
		lcd_wr 		: out 	std_logic;                    	-- Lcd Write control
		lcd_rs 		: out 	std_logic;                    	-- Lcd Data or command control
		lcd_cs 		: out 	std_logic;						-- Lcd Chip select
		cam_href	: in 	std_logic						-- Cam horizontal sync
	);
end CamLcdController;

architecture Behavioral of CamLcdController is
 
component ov7670
	generic (
		Clkfrequency	: integer := 12000000; 	-- 12 MHz main clock
		I2cfrequency    : integer := 100000   	-- 100 kHz sccb frequency
		); 
	port(
		clk			: in std_logic;
		scl			: inout std_logic;
		sda			: inout std_logic;
		cam_conf	: out std_logic				--cam configured
		--leds		: out std_logic_vector(4 downto 0)
		);
end component;
	
component lcd
	port ( 
		clk		: in std_logic;                     -- Clock 12MHz main clock
		rd 		: in std_logic;                    -- Read control
		wr 		: out std_logic;                    -- Write control
		rs 		: out std_logic;                    -- Data or command control
		cs 		: out std_logic;					-- Chip select
		reset	: in std_logic;						--reset lcd 
		lcd_conf	: out std_logic;				--lcd configured 
		wr_strobe: in std_logic;						--Write strobe from controller
		lcd_data  	: out std_logic_vector(7 downto 0);	--Data line 8-bit parallel
		pix_data  	: in std_logic_vector(7 downto 0)	--pixel data line 8-bit parallel
		--leds	: out std_logic_vector(4 downto 0)  --leds for status can be removed	
	   );   
end component;
	
signal lcd_config	: std_logic := '0';				--lcd configured 
signal cam_config	: std_logic := '0';				--cam configured 	
signal lcd_reset	: std_logic := '1';				--lcd reset (hard coded always high)
signal lcd_read		: std_logic := '1';				--lcd read  (hard coded always high) 
signal lcd_strobe 	: std_logic := '0';

begin

u1: ov7670 generic map(12000000,100000) port map(clk,cam_scl,cam_sda,cam_config);
u2: lcd    port map(clk,lcd_read,lcd_wr,lcd_rs,lcd_cs,lcd_reset,lcd_config,lcd_strobe,lcd_data,cam_data);

process(pclk)
begin
-- I am short of GPIO pins on FPGA. Its a development board, not all the pins are exposed.
-- In below if statement vsync is not present you can include it here for better timings.
-- I utilized hsync to grab frame data line by line. 
	if lcd_config = '1' and cam_config = '1' and cam_href = '1' then
		if rising_edge(pclk) and pclk='1' then
			lcd_strobe	<= '0';			-- Write strobe low	
		else 		
			lcd_strobe	<= '1';			-- Write strobe high
		end if;
	end if;

end process;

Mclk <= clk;							-- FPGA clock to camera 12Mhz

end Behavioral;