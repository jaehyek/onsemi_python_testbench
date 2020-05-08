LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;

library UNISIM;
use UNISIM.VComponents.all;

use std.textio.all;
use ieee.std_logic_textio.all; -- require for writing/reading std_logic etc.


ENTITY tb_file IS
END tb_file;

ARCHITECTURE behavior OF tb_file IS

	file input_buf : text; -- text is keyword


	constant period : TIME := 4 ns;

	constant fpga_series         : string  := "7SERIES";
	constant dphy_term_en        : boolean := true; --Enable internal termination on all pairs
	constant d0_invert           : boolean := false;
	constant d1_invert           : boolean := false;
	constant d2_invert           : boolean := false;
	constant d3_invert           : boolean := false;
	constant d0_skew             : natural := 0;
	constant d1_skew             : natural := 0;
	constant d2_skew             : natural := 0;
	constant d3_skew             : natural := 0;
	constant generate_idelayctrl : boolean := true;



	signal reset   : std_logic;
	signal inclk_p : std_logic := '1' ;

	signal serdes_reset   : std_logic;
	signal deser_data     : std_logic_vector(49 downto 0);
	signal byte_clock_int : std_logic;


	signal byte_align_data    : std_logic_vector(49 downto 0);
	signal byte_valid         : std_logic_vector(4 downto 0);
	signal word_align_data    : std_logic_vector(49 downto 0);
	signal packet_end_invalid : std_logic;

	-- csi_rx_packet_handler 
	signal enable               : std_logic;
	signal packet_start         : std_logic;
	signal packet_done          : std_logic;
	signal packet_payload_valid : std_logic;
	signal packet_payload       : std_logic_vector(39 downto 0);
	signal csi_in_frame         : std_logic;
	signal csi_in_line          : std_logic;

	signal word_valid : std_logic;




BEGIN

	inclk_p <= NOT inclk_p after period/2;

	reset          <= '1', '0' after period;
	byte_clock_int <= inclk_p;


	----------------------------------------------------------------------------
	tbfile                           : process
		variable read_col_from_input_buf : line; -- read lines one by one from input_buf

		variable sample0, sample1, trigger : natural;
		variable val_comma                 : character; -- for commas between data in file
		variable good                      : boolean;

		variable file_deser_data     : std_logic_vector ( 49 downto 0 );
		variable file_byte_clock_int : std_logic ;
		variable file_found_hdr4     : std_logic ;
		variable file_found_hdr2     : std_logic ;
		variable file_found_hdr0     : std_logic ;
		variable file_found_hdr1     : std_logic ;
		variable file_found_hdr3     : std_logic ;
		variable file_packet_done1   : std_logic ;
		variable file_serdes_reset   : std_logic ;
		variable file_packet_done    : std_logic ;
	begin
		file_open(input_buf, "D:/proj_baseball/FPGA/baseball_newpcb/iladata.csv", read_mode);
		while not endfile(input_buf) loop
			readline(input_buf, read_col_from_input_buf);
			read(read_col_from_input_buf, sample0, good);
			next when not good; -- i.e. skip the header lines

			read(read_col_from_input_buf, val_comma);
			read(read_col_from_input_buf, sample1, good);
			assert good report "Text I/O read error: sample1" severity ERROR;
			read(read_col_from_input_buf, val_comma);
			read(read_col_from_input_buf, trigger, good);
			assert good report "Text I/O read error: trigger" severity ERROR;

			read(read_col_from_input_buf, val_comma);
			hread(read_col_from_input_buf, file_deser_data, good);
			assert good report "Text I/O read error: file_deser_data" severity ERROR;

			read(read_col_from_input_buf, val_comma);
			read(read_col_from_input_buf, file_byte_clock_int, good);
			assert good report "Text I/O read error: file_byte_clock_int" severity ERROR;

			read(read_col_from_input_buf, val_comma);
			read(read_col_from_input_buf, file_found_hdr4, good);
			assert good report "Text I/O read error: file_found_hdr4" severity ERROR;

			read(read_col_from_input_buf, val_comma);
			read(read_col_from_input_buf, file_found_hdr2, good);
			assert good report "Text I/O read error: file_found_hdr2" severity ERROR;

			read(read_col_from_input_buf, val_comma);
			read(read_col_from_input_buf, file_found_hdr0, good);
			assert good report "Text I/O read error: file_found_hdr0" severity ERROR;

			read(read_col_from_input_buf, val_comma);
			read(read_col_from_input_buf, file_found_hdr1, good);
			assert good report "Text I/O read error: file_found_hdr1" severity ERROR;

			read(read_col_from_input_buf, val_comma);
			read(read_col_from_input_buf, file_found_hdr3, good);
			assert good report "Text I/O read error: file_found_hdr3" severity ERROR;

			read(read_col_from_input_buf, val_comma);
			read(read_col_from_input_buf, file_packet_done1, good);
			assert good report "Text I/O read error: file_packet_done1" severity ERROR;

			read(read_col_from_input_buf, val_comma);
			read(read_col_from_input_buf, file_serdes_reset, good);
			assert good report "Text I/O read error: file_serdes_reset" severity ERROR;

			read(read_col_from_input_buf, val_comma);
			read(read_col_from_input_buf, file_packet_done, good);
			assert good report "Text I/O read error: file_packet_done" severity ERROR;

			deser_data   <= file_deser_data ;
			serdes_reset <= file_serdes_reset ;

			wait for period;

		end loop ;

		file_close(input_buf);
		wait;

	end process ;


	----------------------------------------------------------------------------
	----------------------------------------------------------------------------
	gen_bytealign : for i in 0 to 4 generate
		ba : csi_rx_byte_align
			port map (
				clock              => byte_clock_int,                           -- in 
				reset              => serdes_reset,                             -- in 
				enable             => enable,                                   -- in 
				deser_in           => deser_data((10*i) + 9 downto 10 * i),     -- in 
				packet_start       => packet_start,                             -- in 
				packet_end_invalid => packet_end_invalid,                       -- in 
				byte_valid         => byte_valid(i),                            -- out
				byte_align_data    => byte_align_data((10*i) + 9 downto 10 * i) -- out
			);
	end generate;

	wordalign : csi_rx_word_align
		port map (
			word_clock         => byte_clock_int,     -- in 
			reset              => serdes_reset,       -- in 
			enable             => enable,             -- in 
			packet_end         => packet_end,         -- in 
			packet_start       => packet_start,       -- in 
			packet_end_invalid => packet_end_invalid, -- out
			byte_align_data    => byte_align_data,    -- in 
			byte_valid         => byte_valid,         -- in 
			word_align_data    => word_align_data,    -- out
			word_valid         => word_valid);        -- out


	------------------------------------------------------------------------------
	depacket : csi_rx_packet_handler
		port map (
			clock           => byte_clock_int,       -- in 
			reset           => serdes_reset,         -- in 
			enable          => enable,               -- in 
			i_data          => word_align_data,      -- in 
			i_data_valid    => word_valid,           -- in 
			packet_start    => packet_start,         -- out
			packet_end      => packet_end,           -- out
			o_payload       => packet_payload,       -- out
			o_payload_valid => packet_payload_valid, -- out
			o_frame         => csi_in_frame,         -- out
			o_line          => csi_in_line           -- out
		);
	--------------------------------------------------------------------------------






END behavior;
