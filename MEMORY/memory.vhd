library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;



entity memory is 
	
port(
clk: in std_logic;
--cip select za selekciju modula

ABUS: in std_logic_vector (31 downto 0);
-- linija za podatke
DBUS: inout std_logic_vector(31 downto 0);
		
-- kontrolne linije
wr,rd: in std_logic;

INIT: in std_logic
);
end entity;

architecture behavioral of memory is 

signal cs:std_logic;
signal tmp: std_logic;

begin
cs<=not ABUS(17);
ROM: entity work.rom(behavioral) port map(
clk=>clk,
cs=>cs,
rd=>rd,
data(31 downto 0)=>DBUS(31 downto 0),
address(16 downto 0) => ABUS(16 downto 0),
wr=> init
);
tmp<= init or wr;
RAM: entity work.ram(behavioral) port map(
clk=>clk,
cs=>ABUS(17),
rd=>rd,
data(31 downto 0)=>DBUS(31 downto 0),
address(16 downto 0) => ABUS(16 downto 0),
wr=>tmp
);



end architecture;