library ieee;
use ieee.std_logic_1164.all;

entity MEM_PHASE is
	port
	(
		clk, reset : in std_logic;
		IRin : in std_logic_vector(31 downto 0);
		ALUout_in : in std_logic_vector(31 downto 0);
		
		LS_addr, LS_data : in std_logic_vector(31 downto 0);		-- Load Store instrukcije, adresa i podatak
		LS_l : in std_logic;													-- 1 - load, 0 - store, posle povezati signal cache_wr (negiran LS_l)
		
		PCnew : in std_logic_vector(31 downto 0);
		
		IRout : out std_logic_vector(31 downto 0);
		ALUout_out : out std_logic_vector(31 downto 0);
		PCnew_out : out std_logic_vector(31 downto 0);
		
		Load_out : out std_logic_vector(31 downto 0)					-- izlaz za Load instrukciju, povezati onaj unutrašnji signal Load_out_in_signal
			
	);
end MEM_PHASE;


architecture structural of MEM_PHASE is

	component MEM_WB
	
			port
			(	
				clk, reset : in std_logic;
				IRin : in std_logic_vector(31 downto 0);
				ALUout_in : in std_logic_vector(31 downto 0);
				
				Load_out_in : in std_logic_vector(31 downto 0);
				
				PCnew : in std_logic_vector(31 downto 0);
				
				--------------------
				
				IRout : out std_logic_vector(31 downto 0);
				ALUout_out : out std_logic_vector(31 downto 0);
				PCnew_out : out std_logic_vector(31 downto 0);
				
				Load_out : out std_logic_vector(31 downto 0)
				
			);
		
	end component;
	
	signal Load_out_in_signal : std_logic_vector(31 downto 0);	-- ovo ti je izlaz, tj. linije na koje treba da proslediš
																					-- ono što se čita iz memorije
	signal Data_signal : std_logic_vector(31 downto 0);			-- ovo ti je data ulaz u keš
																					-- ulaz za adresu je LS_addr
	signal cache_wr : std_logic;											-- signal koji govori da li je store ili load

begin

	cache_wr <= not LS_l; 													-- 1 - store, 0 - load
	Data_signal <= LS_data when cache_wr = '1' else
						(others => 'Z');
--	if (cache_wr = '1') then Data_signal <= LS_data;						
--	elsif (cache_wr = '0') then Data_signal <= (others => 'Z');
--	end if;

	MEM_WB_stanglica : component MEM_WB
		
		port map 
		(
				clk => clk, reset => reset,
				IRin => IRin,
				ALUout_in => ALUout_in,
				
				Load_out_in => Load_out_in_signal,
				
				PCnew => PCnew,
				
				--------------------
				
				IRout => IRout,
				ALUout_out => ALUout_out,
				PCnew_out => PCnew_out,
				
				Load_out => Load_out
		);

end structural;
