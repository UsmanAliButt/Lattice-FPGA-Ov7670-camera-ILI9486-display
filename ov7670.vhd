-- Author 	: Usman Ali Butt
-- Dated 	: 11 August 2023 

-- Configuring Ov7670 camera. Ov7670 is configured using sccb protocol.
-- lattice ice40hx1k FPGA is used here and custom sccb ip is designed to
-- read and write ov7670 registers. cam array size 640x480 active pixels.

-- Although i added the register reading functionaly here but its not used.
-- I am only writing to ov7670 registers.  

-- Simple SCCB read cycle. SCCB is almost like I2C. ACK is considered dont care in SCCB.  
-- Below solution is just a simple how to configure SCCB slave with master. 
-- Solution can be optimized e.g.
---- Repeated code can be placed in a function and called every time when required.

-- Lattice ice40 FPGA is used, clock frequency is 12MHz, scaled down to 100KHz for 
-- SCCB.  

-- clk: input clock to FPGA
-- scl: SCCB clock. Its declared as inout can be only in. I declared inout to see if 
-- 		slave stretches the clock. So far no stretching.
-- sda: SCCB sda_bita line. Must be inout and in tristate (open drain). Since sda_bita travels
-- 		between master and slave. If master/slave is done sending, should not hold the bus
-- 		it must be floating when no one is sending sda_bita.   
-- leds: Leds to see status of what is read from slave register. 5 leds so LSB of 8-bit 
-- 		register is displayed on Leds.

-- 					SCCB has weird read cycle. 
-- First write cycle needs to be performed before read. Since write is the only cycle in which register
-- address can be specified. 
-----------------------------------------------------------------------------
--  |Start condition(sda high to low when scl is high) -> 					|
-- 	|	address of slave(7 bit) -> 											|
--	|		Write (0)  -> 													|
--	|			ACK (dont care) -> 											|
-- 	|				register address (8 bit) -> 							|
--	|					stop condition (sda low to high when scl is high) 	|
--  |Start condition(sda high to low when scl is high) -> 					|
-- 	|	address of slave(7 bit) -> 											|
--	|		read (1)  -> 													|
--	|			ACK (dont care) -> 											|
-- 	|				read register  (8 bit) -> 								|
--	|					stop condition (sda low to high when scl is high) 	|
-----------------------------------------------------------------------------
--

-- State machine 
--	idle			: Idle state both scl and sda are high
--	start			: Start of SCCB read/write cycle sda comes low when scl is high
--	addressW		: Slave address for writing 7-bit (8th bit is 0)
--	addressR		: Slave address for reading 7-bit (8th bit is 1)
--	midacknowledge	: dont care ACK bit. Comes between single SCCB cycle
--	acknowledge		: dont care ACK bit. Comes at the end of the single cycle
--	regW			: register address to write 8-bit
--	regR			: register address to read 8-bit
--  regData 		: sda_bita written to register(Actually command)
--	stopadjust		: single dutycycle delay to perfectly allign stop condition.
--	stop			: sda goes low to high in between of scl high. stopadjust alligns sda between scl high.
-- cam_configued	: tell controller that cam is configured

library ieee;
use ieee.std_logic_1164.all;

entity ov7670 is
	generic(
	    Clkfrequency	: integer := 12000000; 	-- 12 MHz
        I2cfrequency    : integer := 100000   	-- 100 kHz		
	);
	port(
		clk				: in std_logic;						-- input clock
		scl				: inout std_logic;					-- sccb clock inout
		sda				: inout std_logic;					-- sccb data inout	
		cam_conf		: out std_logic						-- cam configured signal
		--leds: out std_logic_vector(4 downto 0)
	);
end ov7670;

architecture Behavioral of ov7670 is

-- I2c states in our case
type i2cstate is (idle, start, addressW, addressR, midacknowledge, acknowledge, regW, regR, regData, stopadjust, stop, cam_configued);
signal state,nextstate : i2cstate ;

-- Slave address 
-- LSB is SCCB RW bit, write=0 read=1 
-- MSB 7 bit slave address 
-- hex42 (01000010) is slave address of ov7670 for write
-- hex43 (01000011) is slave address of ov7670 for read
constant slaveaddressW : STD_LOGIC_VECTOR(7 downto 0) := "01000010";
constant slaveaddressR : STD_LOGIC_VECTOR(7 downto 0) := "01000011";
-- Reg 0x0A (00001010) PID of vendor. Must output 0x76
constant Regaddress	   : STD_LOGIC_VECTOR(7 downto 0) := "00001010";
signal   sda_bita 		: STD_LOGIC_VECTOR(7 downto 0) ;


type camconfig is array (0 to 119) of std_logic_vector(7 downto 0); 
constant cam_init : camconfig :=(
								--"01000010","00001100","00000100",   -- 0x21,0x0c,0x04 DCW down sampling enable
								--"01000010","01110000","10111010",	-- 0x21,0x70,0xBA Test pattern output X
								--"01000010","01110001","00110101",	-- 0x21,0x71,0x35 Test pattern output Y
								"01000010","01000001","00011000",	-- 0x21,0x41,0x18 AWB gain enable, Denoise enable, Auto sharpness enable, double matrix
								"01000010","00111101","11011001",	-- 0x21,0x3D,0x gamma bit 7,UV settings bit 1 default 0, saturation enable results best
								"01000010","00111010","00001000",	-- 0x21,0x3A,0x TSLB		UV settings bit 3 default 1
								-- "01000010","01111010","00010000",--"00100000",	-- Gamma slope
								-- "01000010","01111011","00010000",--"00011100",	-- value 28 Gamma Control registers
								-- "01000010","01111100","00011110",--"00101000",	-- value 40 Gamma Control registers
								-- "01000010","01111101","00110101",--"00111100",	-- value 60 Gamma Control registers
								-- "01000010","01111110","01011010",--"01010101",	-- value 85 Gamma Control registers
								-- "01000010","01111111","01101001",--"01101000",	-- value 104 Gamma Control registers
								-- "01000010","10000000","01110110",--"01110110",	-- value 118 Gamma Control registers
								-- "01000010","10000001","10000010",--"10000000",	-- value 128 Gamma Control registers
								-- "01000010","10000010","10001100",--"10001000",	-- value 128 Gamma Control registers
								-- "01000010","10000011","10010110",--"10001111",	-- value 143 Gamma Control registers
								-- "01000010","10000100","10100000",--"10010110",	-- value 150 Gamma Control registers
								-- "01000010","10000101","10110100",--"10100011",	-- value 163 Gamma Control registers
								-- "01000010","10000110","11000011",--"10101111",	-- value 175 Gamma Control registers
								-- "01000010","10000111","11010111",--"11000100",	-- value 196 Gamma Control registers
								-- "01000010","10001000","11100110",--"11010111",	-- value 215 Gamma Control registers
								-- "01000010","10001001","11110100",--"11101000",	-- value 232 Gamma Control registers 1-15 end	
								"01000010","10110000","10000100",	-- 0x21,0xB0,0x84 Why? Some thing colors?
								"01000010","10110001","00001100",	-- 0x21,0xB1,0x0C Why? Some thing colors?
								"01000010","10110010","00001110",	-- 0x21,0xB2,0x0E Why? Some thing colors?
								"01000010","10110011","10000010",	-- 0x21,0xB3,0x82 Why? Some thing colors?
								"01000010","10111000","00001000",	-- 0x21,0xB8,0x08 Why? Some thing colors?
								"01000010","10001100","00000000",   -- 0x21,0x8C,0x00 RGB444 Disable? why?
								-- "01000010","00000000","00001000",   -- 0x21,0x00,0xff General gain, AGC enabled register auto writen by AGC
								"01000010","00010100","01010000",   -- 0x21,0x14,0x50 Gain celling
								"01000010","00000010","11111111",--"00101110",	-- 0x21,0x02,0xF0 red channel gain AWB enabled register auto writen by AWB
								"01000010","01101010","01111111",--"00101000",	-- 0x21,0x6a,0xf0 Green channel gain AWB enabled register auto writen by AWB (6-bit register 7th bit reserved)
								"01000010","00000001","11111111",--"00100110",	-- 0x21,0x01,0xF0 Blue channel gain AWB enabled register auto writen by AWB
								"01000010","10110001","00000100",  	-- 0x21,0xB1,0x04 auto black level compensation 
								--"01000010","00111010","00001100",	-- 0x21,0x3a,0x0C YVYU format selected, default is VYUY
								"01000010","01110110","11100001",	-- 0x21,0x76,0xe1 white black pixel correction enable
								"01000010","01010101","11100010",	-- 0x21,0x55,0x Brightness setting 0x00,0x80(default) means no brightness (6-bit register 7th bit positive/negative)
								"01000010","01010110","00101101",	-- 0x21,0x56,0x Contrast setting 0x40(default) means no contrast
								"01000010","00111011","00010010",	-- 0x21,0x3B,0x12 night mode, auto 50/60Hz and exposure can be less than banding filter 
								-- "01000010","10011110","11111111",	-- 0x21,0x9e,0x3f banding filter value selected from 0x3B register
								"01000010","00010011","10001111",	-- 0x21,0x13,0x8f Enable/disable AGC,AWB,AEC,exposure, banding filter disable
								"01000010","01000011","00001010",	-- 0x21,0x43,0x0a Start AWB 
								"01000010","01000100","11110000",   -- 0x21,0x44,0xf0
								"01000010","01000101","00110100",	-- 0x21,0x45,0x34
								"01000010","01000110","01011000",	-- 0x21,0x46,0x58
								"01000010","01000111","00101000",	-- 0x21,0x47,0x28
								"01000010","01001000","00111010",	-- 0x21,0x48,0x3a
								"01000010","01011001","10001000",	-- 0x21,0x59,0x88
								"01000010","01011010","10001000",	-- 0x21,0x5A,0x88
								"01000010","01011011","01000100",	-- 0x21,0x5B,0x44
								"01000010","01011001","01100111",	-- 0x21,0x5C,0x67
								"01000010","01011010","01001001",	-- 0x21,0x5D,0x49
								"01000010","01011011","00001110",	-- 0x21,0x5E,0x0e End AWB 
								"01000010","01101100","00001010",	-- 0x21,0x6C,0x0a AWB control
								"01000010","01101101","01010101",	-- 0x21,0x6D,0x55 AWB control
								"01000010","01101110","00010001",	-- 0x21,0x6E,0x11 AWB control
								"01000010","01101111","10011110",	-- 0x21,0x6F,0x9f (1 Normal AWB) (0 advance AWB)
								"01000010","01101001","11111111",	-- 0x21,0x69,0x fix gain for Gr,Gb,R,B channels
								"01000010","10101010","10010100",	-- 0x21,0xAA,0x Histogram vs average algorithm for AEC
								-- "01000010","01100110","00000001",	-- 0x21,0x66,0x Lens correction tricky
								-- "01000010","01001111","10000000",--"10110011", 	-- Matrix cofficients
								-- "01000010","01010000","10000000",--"10110011",
								-- "01000010","01010001","00000000",--"00000000",
								-- "01000010","01010010","00100010",--"00111101",
								-- "01000010","01010011","01011110",--"10100111",
								-- "01000010","01010100","10000000",--"11100100", 	-- Matrix cofficients end
								-- "01000010","01011000","00011110", 	-- 0x21,0x58,0x1E Matrix contras center 
								--"01000010","10011111","01111000", 	-- 0x21,0x9f,0x78 High reference luminance
								--"01000010","10100000","01101000", 	-- 0x21,0xa0,0x68 Low reference luminance
								"01000010","00010010","00000001",	-- 0x21,0x12,0x04 RGB , Bayer RGB, Processed Bayer, bar 
								"01000010","01000000","11010000",	-- 0x21,0x40,0xD0 RGB rgb565 with full range 00-FF
								"01000010","00111110","00010001"	-- 0x21,0x3E,0x19 Pclk divided by 2
								--"01000010","01110010","00000000",	-- 0x21,0x72,0x33 Hori and vert dowm sample by 2 
								--"01000010","01110011","00000011"	-- 0x21,0x73,0x0B scalling PCLK div by 2
								);

-- Cycles required for 100Khz frequency production 
constant Clkdivider	 : integer := Clkfrequency 	/ I2cfrequency; -- Full Scl period
constant Sclperiod   : integer := Clkdivider 	/ 2; 			-- Half of scl period 
constant sclmid      : integer := Sclperiod 	/ 2;  			-- Quater 1/4 of scl period 
signal 	 sda_bit	 : std_logic;								-- holds sda value
signal 	 scl_bit	 : std_logic;								-- holds scl value
signal 	 ReaWri	  	 : std_logic;								-- Read Write cycle control 
begin
		
	-- Process to produce 100KHz clock
	process(clk)
	variable counter 	: integer range 0 to Clkdivider; 		-- Full scl period 
	variable count		: integer range 0 to 8 := 8 ;			-- Control to send 8-bits( bit by bit )
	variable config		: integer range 0 to (cam_init'LENGTH) := 0 ;			-- Config array
	variable delay		: integer range 0 to 500 := 0 ; 		-- delay if required can be ommitted 
	variable sclactive 	: std_logic;							-- SCL line status, enable only during sda_bits transfer 
	variable tristateenable  	: STD_LOGIC;			        -- sda line tristate enable 
	variable tristateenablescl  : STD_LOGIC;                    -- scl line tristate enable (can be ommitted)
    begin
        if rising_edge(clk) then
            if counter 	<  Sclperiod then				-- counter less then half scl period enable scl high
                scl_bit 	<= '1';						-- scl high
				if (counter =  sclmid) then             -- counter in mid of scl high start SCCB
					
					if state = idle then
						if delay = 50 then 	
							delay := 0;		
							
							if ReaWri = '1' then
								state <= addressR;
							else	
								state <= addressW;		-- initially jump in the write state
							end if;
							
							sda_bit 			<='0';	-- sda low, start of sccb 
							sclactive			:='1';	-- Enable scl clock
							tristateenable		:='1';	-- sda tristate enable
							tristateenablescl 	:='1';	-- scl tristate enable
						end if;	
						count:=8;
						delay := delay + 1;
					end if;
					
					if state = stop then
						sda_bit 	<=	'1';					-- sda high, end of sccb
						sclactive	:=	'0';					-- Disable scl clock
						if config = (cam_init'LENGTH) then 		-- if cam config complete  					
							state 	<= cam_configued;			-- jump to configured state
						else
							state	<= idle;					-- else start from idle again for next command
						end if;
					end if;
					
					if state = cam_configued then				-- tells main controller that cam is configured
						cam_conf <= '1';
					end if;
				
				end  if;
            elsif counter 	< Clkdivider then           	-- counter less than scl full period 
				if sclactive = '1' then						-- enable scl low
					scl_bit 	<= '0';
				end if;
				
				if (counter = (Sclperiod + sclmid)) then 	-- counter in mid of scl low 
					
					if state = addressW then				-- Write slave address - slave referenced 
					
						case count is
							when 8 =>
								sda_bit <= cam_init(config)(7);
							when 7 =>
								sda_bit <= cam_init(config)(6);
							when 6 =>
								sda_bit <= cam_init(config)(5);
							when 5 =>
								sda_bit <= cam_init(config)(4);
							when 4 =>
								sda_bit <= cam_init(config)(3);
							when 3 =>
								sda_bit <= cam_init(config)(2);
							when 2 =>
								sda_bit <= cam_init(config)(1);
							when 1 =>
								sda_bit <= cam_init(config)(0);	
								state 		<= midacknowledge; 	-- on next clock cycle go to this state
								nextstate	<= regW;			-- After ward's go to this state - register to write
							when others =>
								count := 8;
						end case;

						count := count -1 ;
					end if;
					
					if state = addressR then
					
						case count is
							when 8 =>
								sda_bit <= slaveaddressR(7);
							when 7 =>
								sda_bit <= slaveaddressR(6);
							when 6 =>
								sda_bit <= slaveaddressR(5);
							when 5 =>
								sda_bit <= slaveaddressR(4);
							when 4 =>
								sda_bit <= slaveaddressR(3);
							when 3 =>
								sda_bit <= slaveaddressR(2);
							when 2 =>
								sda_bit <= slaveaddressR(1);
							when 1 =>
								sda_bit <= slaveaddressR(0);	
								state <= midacknowledge;
							when others =>
								count := 8;
						end case;

						count := count -1 ;
					end if;
					
					if state = midacknowledge then			
						-- if ReaWri = '1' then
								-- state <= regR;
								-- tristateenable:='0'; 
							-- else	
								-- state <= regW;
						-- end if;
						state 	<= nextstate;		-- Next state is decided by previous state
						config	:= config + 1;		-- Next byte from init array
						count	:=8;
					end if;
					
					if state = regW then
					
						case count is
							when 8 =>
								sda_bit <= cam_init(config)(7);
							when 7 =>
								sda_bit <= cam_init(config)(6);
							when 6 =>
								sda_bit <= cam_init(config)(5);
							when 5 =>
								sda_bit <= cam_init(config)(4);
							when 4 =>
								sda_bit <= cam_init(config)(3);
							when 3 =>
								sda_bit <= cam_init(config)(2);
							when 2 =>
								sda_bit <= cam_init(config)(1);
							when 1 =>
								sda_bit <= cam_init(config)(0);	
								state 		<= midacknowledge; 	-- on next clock cycle go to this state
								nextstate	<= regData;			-- After ward's go to this state - write sda_bita to register
								ReaWri 		<= '0';				-- Always remain in write state, we dont need read here
							when others =>
								count := 8;
						end case;

						count := count -1 ;
					end if;
					
					if state = regR then
					
						case count is
							when 8 =>
								sda_bita(7) <= sda;
							when 7 =>
								sda_bita(6) <= sda;
							when 6 =>
								sda_bita(5) <= sda;
							when 5 =>
								sda_bita(4) <= sda;
							when 4 =>
								sda_bita(3) <= sda;
							when 3 =>
								sda_bita(2) <= sda;
							when 2 =>
								sda_bita(1) <= sda;
							when 1 =>
								sda_bita(0) <= sda;
								state 	<= acknowledge;
								ReaWri 	<= '0';			-- Back to write							
							when others =>
								count := 8;
						end case;
						

						count := count -1 ;
					end if;
					
					if state = regData then
							
						case count is
							when 8 =>
								sda_bit <= cam_init(config)(7);
							when 7 =>
								sda_bit <= cam_init(config)(6);
							when 6 =>
								sda_bit <= cam_init(config)(5);
							when 5 =>
								sda_bit <= cam_init(config)(4);
							when 4 =>
								sda_bit <= cam_init(config)(3);
							when 3 =>
								sda_bit <= cam_init(config)(2);
							when 2 =>
								sda_bit <= cam_init(config)(1);
							when 1 =>
								sda_bit <= cam_init(config)(0);	
								state 	<= acknowledge;      -- End of write cycle, dont care acknowledge bit of cycle end  
							when others =>
								count := 8;
						end case;

						count := count -1 ;
					
					end if;
					
					if state = acknowledge then
						config	:= config + 1;		-- Next byte from init array(always must be 0x21 for write cycle)
						state 	<= stopadjust;	
					end if;
					
					if state = stopadjust then			-- SCCB Read write cycle stop signal raised				
						tristateenable		:= '1';		-- SDA tristate enable 
						tristateenablescl 	:= '1';		-- SCL tristate enable
						sda_bit 			<= '0';		-- SDA low
						state 				<= stop;	-- Next state stop							
					end if;					
					
				end if;
			else	
                counter := 0;				-- reinitialize counter
            end if;
		counter := counter + 1;				-- counter to generate scl clock
        end if;
		
		if tristateenable = '1' then		-- Sda tristate logic
			sda <= sda_bit;
		else 
			sda <= 'Z';
		end if;
		
		if tristateenablescl = '1' then 	-- Scl tristate logic
			scl <= scl_bit;
		else 
			scl <= 'Z';
		end if;
		
		--leds <= sda_bita(4 downto 0); 			-- Output leds place what ever is read from slave register
    end process;
	

end Behavioral;

