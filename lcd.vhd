-- Author: Usman Ali Butt	
-- Date: 4 August 2023
-- Graphical lcd interfaced with lattice ice40hx1k FPGA. Ov7670 camera video is 
-- displayed on the lcd. Lcd colors are checked using arduino.
-- Lcd controller ILI9486. Size 3.5 inch. pixel 320 x 480
-- Lcd few key parameters
-- Write strobe 50ns-66ns atleast - High 15ns Low 15ns atleast (datasheet says) 
-- Reset, wait 5ms atleast before sending any new command when in sleep in mode (datasheet says)
-- if reset in sleep out mode then wait 120ms after reset, to again send sleep out commnad (datasheet says)

-- Optimization
-- LCD write data and command signals wr,cs,rs can be placed in another process 
-- called from main to writecommand and writedata
-- wrcommand and wrdata states can be made part of lcdstate. Commands counted in
-- wrcommand, given proper delay in single state.

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;


entity lcd is
port ( clk    	: in std_logic;                     --Clock 12MHz
       rd 		: in std_logic;                     --Read control
       wr 		: out std_logic;                	--Write control
       rs 		: out std_logic;                    --Data or command control
	   cs 		: out std_logic;					--Chip select
	   reset	: in std_logic;						--reset lcd 
	   lcd_conf	: out std_logic;					--lcd configured signal
	   wr_strobe: in std_logic;						--Write strobe from controller	
       lcd_data : out std_logic_vector(7 downto 0);	--lcd data line 8-bit parallel
	   pix_data : in std_logic_vector(7 downto 0)	--pixel data line 8-bit parallel
	   --leds		: out std_logic_vector(4 downto 0)  --leds for status can be removed	
	   );   
end lcd;


architecture Behavioral of lcd is 
-- lcd states
-- From idle go to initialize, comdat is to distinguish between initialization
-- command and its data bytes. Strobe is for wr going low to high (50 ns atleast).
-- After initialization lcddata state continously write data to gram. we no more need
-- to write commands.
type lcdstate is (idle, initialize, comdat, strobe, lcddata, lcd_configued);
signal state : lcdstate;
 
-- lcd read write states
-- wrcommnd to write command and wrdata for data writing to lcd. delay is clock cycle delay b/w
-- high low of wr(write) signal. My clock is 83 ns and lcd wr requires 50 ns. 
-- when initialization is done we only need to send data bytes to lcd.   
type lcdrw is (idle, wrcommand, wrdata, delay);
signal rwstate : lcdrw ; 
 
-- lcd config array
-- First two bytes are commnads followed by number of data bytes and then original data bytes e.g 
-- X"00",X"C0",X"02",X"0d",X"0d"   
-- X"00",X"C0" is command  
-- X"02" total number of data bytes in command (here 2)
-- X"0d",X"0d" original data bytes
type lcdconfig is array (0 to 117) of std_logic_vector(7 downto 0); 
constant lcd_init : lcdconfig :=(
								X"00",X"01",X"00",								-- soft reset
								X"00",X"C0",X"02",X"0E",X"0E",					-- Power Control 1
								X"00",X"C1",X"02",X"43",X"00",					-- Power Control 2
								X"00",X"C2",X"01",X"00",						-- Power Control 3
								X"00",X"C5",X"04",X"00",X"48",X"00",X"48",		-- VCOM Control
								X"00",X"E0",X"0F",X"0F",X"24",X"1C",X"0A",X"0F",X"08",X"43",X"88",X"32",X"0F",X"10",X"06",X"0F",X"07",X"00", -- PGAMCTRL (Positive Gamma Control)
								X"00",X"E1",X"0F",X"0F",X"38",X"30",X"09",X"0F",X"0F",X"4E",X"77",X"3C",X"07",X"10",X"05",X"23",X"1B",X"00", -- NGAMCTRL (Negative Gamma Control)
								X"00",X"E2",X"0F",X"0F",X"32",X"2E",X"0B",X"0D",X"05",X"47",X"75",X"37",X"06",X"10",X"03",X"24",X"20",X"00", -- Digital Gamma Control 1
								X"00",X"3A",X"01",X"55", 			-- Color interface
								X"00",X"51",X"01",X"EE",			-- dispaly brightness
								X"00",X"53",X"01",X"2C",			-- brightness, dimming, backlight switched on/off
								X"00",X"20",X"00",					-- display inversion off
								--X"00",X"21",X"00",				-- display inversion on
								X"00",X"29",X"00",					-- display on
								X"00",X"36",X"01",X"00",			-- memory access control (RGB frame 00) (BGR frame 08)
								X"00",X"B4",X"00",		
								X"00",X"B6",X"03",X"00",X"22",X"3B",
								X"00",X"11",X"00",					-- sleep out
								X"00",X"13",X"00",					-- normal dispaly on
								--X"00",X"2A",X"04",X"00",X"00",X"00",X"4F", -- column address x4F=79
								--X"00",X"2B",X"04",X"00",X"00",X"00",X"3B", -- page addresss  x3B=59
								X"00",X"2C",X"00"							-- start writing to memory
								); 								

signal lcd_out: std_logic_vector(7 downto 0);

begin

process(clk)
	variable i 				: integer := 0;							-- Works with delay count			(clock cycle count)
	variable j 				: integer range 0 to lcd_init'LENGTH := 0;		-- Works with data/commands	(fetch next data or command) 
	variable k 				: integer range 0 to 20 := 0;			-- Works with data bytes			(next data byte of command) 
	variable count 			: integer range 0 to 70 := 0;			-- delay count 						(number of clock cycle in delay)
	variable commandcount 	: integer range 0 to 2 := 0; 			-- always two command bytes
	variable datastrobe 	: integer range 0 to 2 := 0;			-- data strobe for proper high low timing
	
begin
	if rising_edge(clk) then
		case state is
			when idle		=>     		-- All controls high initially 
				wr		<= '1';
				rs		<= '1';
				cs		<= '1';
				--reset <= '1';			-- Constant high(you can enable disable it)
				count 	:= 50;			-- 5ms delay for lcd power up(even less worked for me) depends on your clock
				state 	<= strobe;
			when initialize =>			-- start with writing command
				state 	<= comdat;
				rwstate <= wrcommand;
			when comdat		=>
				case rwstate is
					when wrcommand 	=>			-- fetch and write command
						wr		<= '0';
						rs		<= '0';
						cs		<= '0';
						count 	:= 50;--69000;	-- change delay according to your needs
						state 	<= strobe;		-- command data placed on line, now jump to strobe 
						k 		:= 0;			-- k reset - Used to count number of data bytes of command
						lcd_out <= lcd_init(j);	-- command byte fetched from array
						--leds	<= "10000";
					
					when wrdata		=>			-- fetch and write data
						if k = to_integer(unsigned(lcd_init(j))) then
							if (j+k) >= lcd_init'LENGTH - 1  then  	-- if all initialization is done 
								state <= lcd_configued;				-- jump to lcd configured state
							else	
								j		:=j+k+1; 	 				-- indexing next command
								state 	<= initialize;   			-- move back to initialize to start new command
							end if;					
						else
							wr		<= '0';			-- fetch and write data
							rs		<= '1';
							cs		<= '0';
							count 	:= 50;			-- change delay according to your needs
							state 	<= strobe;		-- command data placed on line, now jump to strobe 
							k		:=k+1;  		-- config data is placed on next iteration of j so j+k
							lcd_out <= lcd_init(j+k);  -- data byte fetched from array
							--leds	<= "00001";
						end if;
					
					when idle		=>
					when delay 		=>				--So far useless in next phase will be utilized
				end case;
			when strobe		=>			
				if i = count then		-- count for delay purposes
					i 	:= 0;			-- back to zero for next delay 
					wr	<= '1';			-- write goes high and data/command is writen in Gram of lcd
					--leds	<= "00000";	
					
					case rwstate is
						when wrcommand 	=>							
							rwstate <= delay;  					-- After writing command byte give delay 
							j 		:= j+1;  					-- next command indexed
							commandcount := commandcount + 1; 	-- command count increased (commands are 16 bits wide)
						when delay =>							-- delay necessary after strobing and next command/data select
		
							if commandcount > 1 then 			-- Two command bytes sent before jumping to data bytes
								rwstate <= wrdata;				-- commands completed jump to data bytes
								commandcount := 0;				-- command count 0
							else
								rwstate <= wrcommand;			-- else back to command
							end if;	

							state   <= comdat;					-- after delay always jump to command/data selection state
							
						when wrdata		=>
							rwstate <= delay;  -- After writing data give delay	
							commandcount := 2; -- No more commands by pass command check in dealy
						when idle		=>
							state <= initialize; -- From idle always go to initialize state					
					end case;
					
				else
					i := i+1;		-- increment clock cycle count
				end if;
				
			when lcddata 	=>				-- send cam pixel data to lcd
				if wr_strobe = '0' then
					wr		<= '0';			-- Write strobe low
					lcd_out <= pix_data ; 	-- pixels comming from cam 
				else 
					wr	<= '1';				-- Write strobe high	
				end if;			
							
			when lcd_configued	=>
				lcd_conf<= '1';			-- lcd configured signal
				rs		<= '1';			-- Data register selected - now we only send data
				state 	<= lcddata;		-- Always remain in lcddata state
			
		end case; --end main case
	
	end if;  -- end main if clock event
end process;

lcd_data <= lcd_out;			-- data assigned lcd_out(send command/config/cam pixels to lcd)

end Behavioral;
