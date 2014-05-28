library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;



entity rom is 
port(
clk: in std_logic;
--cip select za selekciju modula
cs:in std_logic;

address: in std_logic_vector (16 downto 0);
-- linija za podatke
data: inout std_logic_vector(31 downto 0);
		
-- kontrolne linije
wr,rd: in std_logic
);

end entity;

architecture behavioral of rom is 

signal cs0:std_logic;
signal cs1:std_logic;
signal cs2:std_logic;
signal cs3:std_logic;

begin

cs0<= (not address(3)) and (not address(2)) and cs;
MODULE_0: entity work.module(behavioral) port map (
		clk =>clk,
		data => data,
		wr =>wr,
		rd =>rd,
		cs => cs0,
		address => address(16 downto 4)
		);
cs1<= (not address(3)) and  address(2) and cs;
MODULE_1: entity work.module(behavioral) port map (
		clk =>clk,
		data => data,
		wr =>wr,
		rd =>rd,
		cs => cs1,
		address => address(16 downto 4)
		);
cs2<= address(3) and (not address(2)) and cs;
MODULE_2: entity work.module(behavioral) port map (
		clk =>clk,
		data => data,
		wr =>wr,
		rd =>rd,
		cs => cs2,
		address => address(16 downto 4)
		);
cs3<=  address(3) and address(2) and cs;
MODULE_3: entity work.module(behavioral) port map (
		clk =>clk,
		data => data,
		wr =>wr,
		rd =>rd,
		cs => cs3,
		address => address(16 downto 4)
		);
		


end architecture;