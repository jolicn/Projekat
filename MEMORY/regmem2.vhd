library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;


entity regmem2 is 
	
port(
clk: in std_logic;
--cip select za selekciju modula
cs:in std_logic;

address: in std_logic_vector (12 downto 0); 
-- linija za podatke
data: inout std_logic_vector(31 downto 0);
		
-- kontrolne linije
wr,rd: in std_logic
);
end entity;

architecture behavioral of regmem2 is 

type memory_array is array (0 to (2**13)-1) of std_logic_vector(31 downto 0);
signal memory:memory_array;

signal reading: std_logic :='0' ;
signal writing: std_logic :='0' ;

signal tmpData: std_logic_vector(31 downto 0);
signal tmpAddress: std_logic_vector(12 downto 0);

begin

process(clk) is
	begin
		
		if(rising_edge(clk)) then
		data <= "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ";
						
			if( wr = '1' and cs='1' and writing='0' and reading='0'  ) then
				writing<='1';			
				tmpAddress<=address;
				tmpData<=data;
			end if;
					
		
			if( rd = '1' and cs='1' and writing='0' and reading='0'  ) then
				reading<='1';
				tmpAddress<=address;
			end if;
			
			if ( writing='1' ) then
			
				
				writing<='0';
				memory(conv_integer(tmpAddress)) <= tmpData;
			
			end if;

			if ( reading='1' ) then
			
				
				reading<='0';
				 data <= memory(conv_integer(tmpAddress));
			
			end if;
			
			end if;	
		
		
end process;
end architecture;