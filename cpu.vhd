-- cpu.vhd: Simple 8-bit CPU (BrainFuck interpreter)
-- Copyright (C) 2022 Brno University of Technology,
--                    Faculty of Information Technology
-- Author(s): Vladimir Hucovic <xhucov00@stud.fit.vutbr.cz>
--
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

-- ----------------------------------------------------------------------------
--                        Entity declaration
-- ----------------------------------------------------------------------------
entity cpu is
 port (
   CLK   : in std_logic;  -- hodinovy signal
   RESET : in std_logic;  -- asynchronni reset procesoru
   EN    : in std_logic;  -- povoleni cinnosti procesoru
 
   -- synchronni pamet RAM
   DATA_ADDR  : out std_logic_vector(12 downto 0); -- adresa do pameti
   DATA_WDATA : out std_logic_vector(7 downto 0); -- mem[DATA_ADDR] <- DATA_WDATA pokud DATA_EN='1'
   DATA_RDATA : in std_logic_vector(7 downto 0);  -- DATA_RDATA <- ram[DATA_ADDR] pokud DATA_EN='1'
   DATA_RDWR  : out std_logic;                    -- cteni (0) / zapis (1)
   DATA_EN    : out std_logic;                    -- povoleni cinnosti
   
   -- vstupni port
   IN_DATA   : in std_logic_vector(7 downto 0);   -- IN_DATA <- stav klavesnice pokud IN_VLD='1' a IN_REQ='1'
   IN_VLD    : in std_logic;                      -- data platna
   IN_REQ    : out std_logic;                     -- pozadavek na vstup data
   
   -- vystupni port
   OUT_DATA : out  std_logic_vector(7 downto 0);  -- zapisovana data
   OUT_BUSY : in std_logic;                       -- LCD je zaneprazdnen (1), nelze zapisovat
   OUT_WE   : out std_logic                       -- LCD <- OUT_DATA pokud OUT_WE='1' a OUT_BUSY='0'
 );
end cpu;


-- ----------------------------------------------------------------------------
--                      Architecture declaration
-- ----------------------------------------------------------------------------
architecture behavioral of cpu is

--- Program Counter signals

signal PC : std_logic_vector(12 downto 0);
signal PC_inc : std_logic;
signal PC_dec : std_logic;


--- Loop Counter signals

signal CNT : std_logic_vector(12 downto 0);
signal CNT_inc : std_logic;
signal CNT_dec : std_logic;


--- Memory pointer signals

signal PTR : std_logic_vector(12 downto 0);
signal PTR_inc : std_logic;
signal PTR_dec : std_logic;

--- Multiplexor selector signals

signal mx1_sel : std_logic;
signal mx2_sel : std_logic_vector(1 downto 0);

--- FSM states

type FSMstate is(s_reset,
				s_fetch, 
				s_decode,
				s_ptr_inc,
				s_ptr_dec,
    			s_value_inc1, s_value_inc2,
				s_value_dec1, s_value_dec2,
				s_print1, s_print2,
				s_load1, s_load2,
				s_while1, s_while2, s_while3,
				s_while_end1, s_while_end2, s_while_end3, s_while_end4, s_while_end5,
				s_do_while, s_do_while_end1, s_do_while_end2, s_do_while_end3, s_do_while_end4, s_do_while_end5,
				 s_null);
signal pstate : FSMstate;
signal nstate : FSMstate;

begin

-- Program counter
reg_PC: process (RESET, CLK, PC_inc, PC_dec)
begin
	if(RESET = '1') then
		PC <= (others => '0');
	elsif (CLK'event) and (CLK = '1') then
		if(PC_inc='1') then
			PC <= PC + 1;
		elsif(PC_dec='1') then
			PC <= PC - 1;
		end if;
	end if;
end process;

-- CNT register
reg_CNT: process (RESET, CLK, CNT_inc, CNT_dec)
begin
	if(RESET = '1') then
		CNT <= (others => '0');
	elsif (CLK'event) and (CLK = '1') then
		if(CNT_inc='1') then
			CNT <= CNT + 1;
		elsif(CNT_dec='1') then
			CNT <= CNT - 1;
		end if;
	end if;
end process;

-- PTR register
reg_PTR: process (RESET, CLK, PTR_inc, PTR_dec)
begin
	if(RESET = '1') then
		PTR <= conv_std_logic_vector(4096, 13);
	elsif (CLK'event) and (CLK = '1') then
		if(PTR_inc='1') then
			PTR <= conv_std_logic_vector(4096 + (conv_integer(unsigned(PTR))+1) mod 4096, 13);
		elsif(PTR_dec='1') then 
			PTR <= conv_std_logic_vector(4096 + (conv_integer(unsigned(PTR))-1) mod 4096, 13);
		end if;
	end if;
end process;

--MUX that sends either PTR or PC to DATA_ADDR
with mx1_sel select
	DATA_ADDR <= PC when '0',
			PTR when '1',
			(others => '0') when others;

-- MUX that sends either keyboard input, RDATA+1 or RDATA-1 to DATA_WDATA
with mx2_sel select
	DATA_WDATA <= IN_DATA when "00",
			(DATA_RDATA - 1) when "01",
			(DATA_RDATA + 1) when "10",
			(others => '0') when others;

psreg: process (RESET, CLK, EN)
begin
	if(RESET='1') then
		pstate <= s_reset;
	elsif(CLK'event and CLK='1' and EN='1') then
		pstate <= nstate;
	end if;
end process;

-- FSM Next state logic, output logic
nslogic: process(pstate, IN_VLD, OUT_BUSY, DATA_RDATA, CNT, EN)
begin
	IN_REQ <= '0';
	OUT_DATA <= (others => '0');
	OUT_WE <= '0';
	mx1_sel <= '0';
	mx2_sel <= (others => '0');
	DATA_RDWR <= '0';
	DATA_EN <= '0';
	PC_inc <= '0';
	PC_dec <= '0';
	PTR_inc <= '0';
	PTR_dec <= '0';
	CNT_inc <= '0';
	CNT_dec <= '0';

	case pstate is
		when s_null =>
			nstate <= s_null;
		when s_reset =>
			nstate <= s_fetch;
		when s_fetch =>
			mx1_sel <= '0';
			DATA_EN <= '1';
			nstate <= s_decode;
		when s_decode =>
			case DATA_RDATA is
				when X"00" =>
					nstate <= s_null;
				when X"3E" =>
					nstate <= s_ptr_inc;
				when X"3C" =>
					nstate <= s_ptr_dec;
				when X"2B" =>
					nstate <= s_value_inc1;
				when X"2D" =>
					nstate <= s_value_dec1;
				when X"2E" =>
					nstate <= s_print1;
				when X"2C" =>
					nstate <= s_load1;
				when X"5B" =>
					nstate <= s_while1;
				when X"5D" =>
					nstate <= s_while_end1;
				when X"28" =>
					nstate <= s_do_while;
				when X"29" =>
					nstate <= s_do_while_end1;
				when X"0" =>
					nstate <= s_null;
				when others =>
					PC_inc <= '1';
					nstate <= s_fetch;
			end case;
		when s_ptr_inc =>
			PTR_inc <= '1';
			PC_inc <= '1';
			nstate <= s_fetch;

		when s_ptr_dec =>
			PTR_dec <= '1';
			PC_inc <= '1';
			nstate <= s_fetch;		

		-- send content of PTR to DATA_ADDR and read the value from DATA_RDATA
		when s_value_inc1 =>
			mx1_sel <= '1';
			DATA_EN <= '1';
			DATA_RDWR <= '0';
			nstate <= s_value_inc2;
	

		-- Increment the value read by 1 and write to DATA_WDATA, increment PC and 
		-- fetch next instruction
		when s_value_inc2 =>
			mx2_sel <= "10";
			mx1_sel <= '1';
			DATA_RDWR <= '1';
			DATA_EN <= '1';	

			PC_inc <= '1';
			nstate <= s_fetch;

			-- send content of PTR to DATA_ADDR and read the value from DATA_RDATA
		when s_value_dec1 =>
			mx1_sel <= '1';
			DATA_EN <= '1';
			DATA_RDWR <= '0';
			nstate <= s_value_dec2;
	

		-- Increment the value read by 1 and write to DATA_WDATA, increment PC and 
		-- fetch next instruction
		when s_value_dec2 =>
			mx2_sel <= "01";
			mx1_sel <= '1';
			DATA_RDWR <= '1';
			DATA_EN <= '1';	

			PC_inc <= '1';
			nstate <= s_fetch;	


		when s_load1 =>
			IN_REQ <= '1';
			nstate <= s_load2;
		when s_load2 =>
			if(IN_VLD = '1') then
				mx2_sel <= "00";
				mx1_sel <= '1';
				DATA_RDWR <= '1';
				DATA_EN <= '1';
				PC_inc <= '1';
				nstate <= s_fetch;
			else 
				nstate <= s_load1;
			end if;
		
		when s_print1 =>
			mx1_sel <= '1';
			DATA_EN <= '1';
			if(OUT_BUSY = '1') then
				nstate <= s_print1;
			else
				nstate <= s_print2;
			end if;	
		when s_print2 =>
			OUT_DATA <= DATA_RDATA;
			OUT_WE <= '1';
			PC_inc <= '1';
			nstate <= s_fetch;


		-- Prepare to read data from PTR
		when s_while1 =>
			PC_inc <= '1';
			mx1_sel <= '1';
			DATA_EN <= '1';
			nstate <= s_while2;

		-- Read data from PTR
		when s_while2 =>
			mx1_sel <= '1';
			-- If *PTR = 0, jump behind the loop
			if(DATA_RDATA = 0) then
				CNT_inc <= '1';
				DATA_EN <= '1';
				nstate <= s_while3;
			-- if *PTR != 0, execute instructions inside the loop
			else 
				nstate <= s_fetch;	
			end if;
				
		-- Skip instructions until PC gets behind the loop end symbol ']'; count the loop start symbols '['
		-- to take care of nested loops	
		when s_while3 =>
				if(CNT = 0) then
					nstate <= s_fetch;
				else
					DATA_EN <= '1';
					if(DATA_RDATA = X"5B") then
						CNT_inc <= '1';
					elsif (DATA_RDATA =X"5D") then
						CNT_dec <= '1';
					end if;	
					PC_inc <= '1';
					nstate <= s_while3;
				end if;
		
		-- Prepare to read data from PTR	
		when s_while_end1 =>
				mx1_sel <= '1';
				DATA_EN <= '1';
				nstate <= s_while_end2;

		-- Read from RDATA and decide wheter to continue to loop or break
		when s_while_end2 =>
				-- if *PTR = 0, break the loop (continue to the next instruction)
				if(DATA_RDATA = 0) then
					PC_inc <= '1';
					nstate <= s_fetch;
				-- else jump to the start of the loop	
				else
					CNT_inc <= '1';
					PC_dec <= '1';
					DATA_EN <= '1';
					nstate <= s_while_end3;
				end if;	

				-- 1 clock space for PC to be updated
		when s_while_end3 =>
				DATA_EN <= '1';
				nstate <= s_while_end4;
				
				-- Check if *PC is the start of the loop '[' ; count the loop end symbols '['
				-- to take care of nested loops
		when s_while_end4 =>
				if(CNT = 0) then
					nstate <= s_fetch;
				else 
					DATA_EN <= '1';
					if(DATA_RDATA = X"5D" ) then 
						CNT_inc <= '1';
					elsif (DATA_RDATA = X"5B") then
						CNT_dec <= '1';
					end if;
				end if;
				nstate <= s_while_end5;
				
				-- If the opening bracket of the respective loop was found, continue with the next instruction
				-- otherwise continue looking for it
		when s_while_end5 =>
				if(CNT = 0) then
					PC_inc <= '1';
					nstate <= s_fetch;
				else 
					DATA_EN <= '1';
					PC_dec <= '1';
					nstate <= s_while_end4;
				end if;

				-- Just skip the do-while opening bracket
		when s_do_while =>
				PC_inc <= '1';
				nstate <= s_fetch;

		-- Prepare to read data from PTR	
		when s_do_while_end1 =>
				mx1_sel <= '1';
				DATA_EN <= '1';
				nstate <= s_do_while_end2;

		-- Read from RDATA and decide wheter to continue to loop or break
		when s_do_while_end2 =>
				-- if *PTR = 0, break the loop (continue to the next instruction)
				if(DATA_RDATA = 0) then
					PC_inc <= '1';
					nstate <= s_fetch;
				-- else jump to the start of the loop	
				else
					CNT_inc <= '1';
					PC_dec <= '1';
					DATA_EN <= '1';
					nstate <= s_do_while_end3;
				end if;	

				-- 1 clock space for PC to be updated
		when s_do_while_end3 =>
				DATA_EN <= '1';
				nstate <= s_do_while_end4;
				
				-- Check if *PC is the start of the loop '(' ; count the loop end symbols '('
				-- to take care of nested loops
		when s_do_while_end4 =>
				if(CNT = 0) then
					nstate <= s_fetch;
				else 
					DATA_EN <= '1';
					if(DATA_RDATA = X"29" ) then 
						CNT_inc <= '1';
					elsif (DATA_RDATA = X"28") then
						CNT_dec <= '1';
					end if;
				end if;
				nstate <= s_do_while_end5;
				
				-- If the opening bracket of the respective loop was found, continue with the next instruction
				-- otherwise continue looking for it
		when s_do_while_end5 =>
				if(CNT = 0) then
					PC_inc <= '1';
					nstate <= s_fetch;
				else 
					DATA_EN <= '1';
					PC_dec <= '1';
					nstate <= s_do_while_end4;
				end if;
				


		when others => null;	
				
				
	end case;		
end process;

end behavioral;
