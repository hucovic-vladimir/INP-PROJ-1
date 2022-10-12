-- cpu.vhd: Simple 8-bit CPU (BrainFuck interpreter)
-- Copyright (C) 2022 Brno University of Technology,
--                    Faculty of Information Technology
-- Author(s): Vladimír Hucovič <xhucov00@stud.fit.vutbr.cz>
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

type FSMstate is(s_reset, s_fetch, s_decode, s_ptr_inc, s_ptr_dec, s_value_inc1, s_value_inc2, s_value_inc3, s_value_inc4, s_value_dec1, s_value_dec2, s_value_dec3, s_value_dec4, s_print, s_load, s_null);
signal pstate : FSMstate;
signal nstate : FSMstate;

begin


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


mx1: process(CLK, RESET, mx1_sel)
begin
	if(RESET = '1') then 
		DATA_ADDR <= (others => '0');
	elsif (CLK'event and CLK='1') then
		case mx1_sel is
			when '0' => DATA_ADDR <= PC;
			when '1' => DATA_ADDR <= PTR;
			when others => DATA_ADDR <= (others => '0');
		end case;
	end if;			
end process;

mx2: process(CLK, RESET, mx2_sel)
begin
	if(RESET = '1') then
		DATA_WDATA <= (others => '0');
	elsif (CLK'event and CLK='1') then
		case mx2_sel is
			when "00" => DATA_WDATA <= IN_DATA;
			when "01" => DATA_WDATA <= (DATA_RDATA - 1);
			when "10" => DATA_WDATA <= (DATA_RDATA + 1);
			when others => DATA_WDATA <= (others => '0');
		end case;
	end if;
end process;

psreg: process (RESET, CLK, EN)
begin
	if(RESET='1') then
		pstate <= s_reset;
	elsif(CLK'event and CLK='1' and EN='1') then
		pstate <= nstate;
	end if;
end process;



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
					nstate <= s_print;
				when X"2C" =>
					nstate <= s_load;
				when others =>
					nstate <= s_null;
			end case;
		when s_ptr_inc =>
				PTR_inc <= '1';
				PC_inc <= '1';
				nstate <= s_fetch;

		when s_ptr_dec =>
				PTR_dec <= '1';
				PC_inc <= '1';
				nstate <= s_fetch;		

		-- send content of PTR to DATA_ADDR (DATA_ADDR = ptr)
		when s_value_inc1 =>
				mx1_sel <= '1';
				nstate <= s_value_inc2;
		
		-- Read from DATA_ADDR (DATA_RDATA = *ptr) 		
		when s_value_inc2 =>
				DATA_EN <= '1';
				DATA_RDWR <= '0';
				mx1_sel <= '1';
				nstate <= s_value_inc3;

		-- Increment the value of ptr  (*ptr)++ 
		when s_value_inc3 =>
				mx2_sel <= "10";
				mx1_sel <= '1';

		-- We can also increment PC here
				PC_inc <= '1';
				nstate <= s_value_inc4;

		-- Send the incremented value to DATA_WDATA (DATA_WDATA = *ptr)		
		when s_value_inc4 =>
				DATA_RDWR <= '1';
				DATA_EN <= '1';	
				mx2_sel <= "10";
				nstate <= s_fetch;

		-- send content of PTR to DATA_ADDR (DATA_ADDR = ptr)
		when s_value_dec1 =>
				mx1_sel <= '1';
				nstate <= s_value_dec2;

		-- Read from DATA_ADDR (DATA_RDATA = *ptr) 		
		when s_value_dec2 =>
				DATA_EN <= '1';
				DATA_RDWR <= '0';
				mx1_sel <= '1';
				nstate <= s_value_dec3;

		-- Decrement the value of ptr  (*ptr)-- 
		when s_value_dec3 =>
				mx2_sel <= "01";
				mx1_sel <= '1';

		-- We can increment PC here		
				PC_inc <= '1';
				nstate <= s_value_dec4;

		-- Send the decremented value to DATA_WDATA (DATA_WDATA = *ptr)		
		when s_value_dec4 =>
				DATA_RDWR <= '1';
				DATA_EN <= '1';	
				mx2_sel <= "01";
				nstate <= s_fetch;		
		when others => null;		
				
	end case;		
end process;


--with mx1_sel select
--	DATA_ADDR <= PC when '0',
--			PTR when '1',
--			(others => '0') when others;

--with mx2_sel select
--	DATA_WDATA <= IN_DATA when "00",
--			(DATA_RDATA - 1) when "01",
--			(DATA_RDATA + 1) when "10",
--			(others => '0') when others;

end behavioral;
