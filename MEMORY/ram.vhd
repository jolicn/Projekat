library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;



entity ram is 
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

architecture behavioral of ram is 

signal cs4:std_logic;
signal cs5:std_logic;
signal cs6:std_logic;
signal cs7:std_logic;

begin

cs4<= (not address(3)) and (not address(2)) and cs;
MODULE_4: entity work.module(behavioral) port map (
		clk =>clk,
		data => data,
		wr =>wr,
		rd =>rd,
		cs => cs4,
		address => address(16 downto 4)
		);
cs5<= (not address(3)) and  address(2) and cs;
MODULE_5: entity work.module(behavioral) port map (
		clk =>clk,
		data => data,
		wr =>wr,
		rd =>rd,
		cs => cs5,
		address => address(16 downto 4)
		);
cs6<= address(3) and (not address(2)) and cs;
MODULE_6: entity work.module(behavioral) port map (
		clk =>clk,
		data => data,
		wr =>wr,
		rd =>rd,
		cs => cs6,
		address => address(16 downto 4)
		);
cs7<=  address(3) and address(2) and cs;
MODULE_7: entity work.module(behavioral) port map (
		clk =>clk,
		data => data,
		wr =>wr,
		rd =>rd,
		cs => cs7,
		address => address(16 downto 4)
		);
		


end architecture;