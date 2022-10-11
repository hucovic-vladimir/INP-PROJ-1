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



begin




reg_PC: process (RESET, CLK)
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



reg_CNT: process (RESET, CLK)
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


reg_PTR: process (RESET, CLK)
begin
	if(RESET = '1') then
		PTR <= (others => '0');
	elsif (CLK'event) and (CLK = '1') then
		if(PTR_inc='1') then
			PTR <= PTR + 1;
		elsif(PTR_dec='1') then
			PTR <= PTR - 1;
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

	--DATA_ADDR <= PC when (mx1_sel = '0')
	--				else CNT;
end process;
DATA_WDATA <= IN_DATA when (mx2_sel = "00") else
			(DATA_RDATA - 1) when (mx2_sel = "01") else
			(DATA_RDATA + 1) when (mx2_sel = "10") else
			(others => '0');
end behavioral;
