library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;



entity module is 
	
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

architecture behavioral of module is 

--proba
signal data12 : std_logic_vector(31 downto 0);
signal data23 : std_logic_vector(31 downto 0);
signal data34 : std_logic_vector(31 downto 0);
signal data45 : std_logic_vector(31 downto 0);
signal data56 : std_logic_vector(31 downto 0);
signal data67 : std_logic_vector(31 downto 0);
signal data78 : std_logic_vector(31 downto 0);
signal data89 : std_logic_vector(31 downto 0);
signal data910 : std_logic_vector(31 downto 0);
signal data1011 : std_logic_vector(31 downto 0);
signal data1112 : std_logic_vector(31 downto 0);
signal cs12 : std_logic;
signal wr12 : std_logic;
signal rd12 : std_logic;
signal cs23 : std_logic;
signal wr23 : std_logic;
signal rd23 : std_logic;
signal cs34 : std_logic;
signal wr34 : std_logic;
signal rd34 : std_logic;
signal cs45 : std_logic;
signal wr45 : std_logic;
signal rd45 : std_logic;
signal cs56 : std_logic;
signal wr56 : std_logic;
signal rd56 : std_logic;
signal cs67 : std_logic;
signal wr67 : std_logic;
signal rd67 : std_logic;
signal cs78 : std_logic;
signal wr78 : std_logic;
signal rd78 : std_logic;
signal cs89 : std_logic;
signal wr89 : std_logic;
signal rd89 : std_logic;
signal cs910 : std_logic;
signal wr910 : std_logic;
signal rd910 : std_logic;
signal cs1011 : std_logic;
signal wr1011 : std_logic;
signal rd1011 : std_logic;
signal cs1112 : std_logic;
signal wr1112 : std_logic;
signal rd1112 : std_logic;
signal address12 : std_logic_vector (12 downto 0);
signal address23 : std_logic_vector (12 downto 0);
signal address34 : std_logic_vector (12 downto 0);
signal address45 : std_logic_vector (12 downto 0);
signal address56 : std_logic_vector (12 downto 0);
signal address67 : std_logic_vector (12 downto 0);
signal address78 : std_logic_vector (12 downto 0);
signal address89 : std_logic_vector (12 downto 0);
signal address910 : std_logic_vector (12 downto 0);
signal address1011 : std_logic_vector (12 downto 0);
signal address1112 : std_logic_vector (12 downto 0);

begin

				ENT1: entity work.regmem(arch) port map (
				clk => clk,
				data_in => data,
				data_out => data12,
				wr=>wr,
				rd=>rd,
				cs=>cs,
				csout=>cs12,
				wrout=>wr12,
				rdout=>rd12,
				address => address,
				addressout =>address12
				);
				ENT2: entity work.regmem(arch) port map (
				clk => clk,
				data_in => data12,
				data_out => data23,
				wr=>wr12,
				rd=>rd12,
				cs=>cs12,
				csout=>cs23,
				wrout=>wr23,
				rdout=>rd23,
				address => address12,
				addressout =>address23
				);	
				ENT3: entity work.regmem(arch) port map (
			   clk => clk,
			   data_in => data23,
			   data_out => data34,
				wr=>wr23,
				rd=>rd23,
				cs=>cs23,
				csout=>cs34,
				wrout=>wr34,
				rdout=>rd34,
				address => address23,
				addressout =>address34
		      );	
				ENT4: entity work.regmem(arch) port map (
				clk => clk,
				data_in => data34,
				data_out => data45,
				wr=>wr34,
				rd=>rd34,
				cs=>cs34,
				csout=>cs45,
				wrout=>wr45,
				rdout=>rd45,
				address => address34,
				addressout =>address45
				);	
				ENT5: entity work.regmem(arch) port map (
				clk => clk,
				data_in => data45,
				data_out => data56,
				wr=>wr45,
				rd=>rd45,
				cs=>cs45,
				csout=>cs56,
				wrout=>wr56,
				rdout=>rd56,
				address => address45,
				addressout =>address56
				);	
				ENT6: entity work.regmem(arch) port map (
				clk => clk,
				data_in => data56,
				data_out => data67,
				wr=>wr56,
				rd=>rd56,
				cs=>cs56,
				csout=>cs67,
				wrout=>wr67,
				rdout=>rd67,
				address => address56,
				addressout =>address67
				);	
				ENT7: entity work.regmem(arch) port map (
				clk => clk,
				data_in => data67,
				data_out => data78,
				wr=>wr67,
				rd=>rd67,
				cs=>cs67,
				csout=>cs78,
				wrout=>wr78,
				rdout=>rd78,
				address => address67,
				addressout =>address78
				);	
				ENT8: entity work.regmem(arch) port map (
				clk => clk,
				data_in => data78,
				data_out => data89,
				wr=>wr78,
				rd=>rd78,
				cs=>cs78,
				csout=>cs89,
				wrout=>wr89,
				rdout=>rd89,
				address => address78,
				addressout =>address89
				);	
				ENT9: entity work.regmem(arch) port map (
				clk => clk,
				data_in => data89,
				data_out => data910,
				wr=>wr89,
				rd=>rd89,
				cs=>cs89,
				csout=>cs910,
				wrout=>wr910,
				rdout=>rd910,
				address => address89,
				addressout =>address910
				);	
				ENT10: entity work.regmem(arch) port map (
				clk => clk,
				data_in => data910,
				data_out => data1011,
				wr=>wr910,
				rd=>rd910,
				cs=>cs910,
				csout=>cs1011,
				wrout=>wr1011,
				rdout=>rd1011,
				address => address910,
				addressout =>address1011
				);
				ENT11: entity work.regmem(arch) port map (
				clk => clk,
				data_in => data1011,
				data_out => data1112,
				wr=>wr1011,
				rd=>rd1011,
				cs=>cs1011,
				csout=>cs1112,
				wrout=>wr1112,
				rdout=>rd1112,
				address => address1011,
				addressout =>address1112
				);
				
				ENT12: entity work.regmem2(behavioral) port map (
				clk => clk,
				data => data1112,
				wr=>wr1112,
				rd=>rd1112,
				cs=>cs1112,
				address => address1112
				);


end architecture;