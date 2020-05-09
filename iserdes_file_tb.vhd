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


	constant period : TIME := 2.78 ns;

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

	component csi_rx_byte_align is
		port (
        clock              : in  STD_LOGIC;                      --byte clock in, word_clock_int, align?´ ?˜ì§? ?žˆì§? ?•Š?‹¤.
        reset              : in  STD_LOGIC;                      --from serdes_reset, synchronous active high reset
        enable             : in  STD_LOGIC;                      --active high enable
        deser_in           : in  STD_LOGIC_VECTOR (9 downto 0);  --raw data from ISERDES
        packet_start       : in  STD_LOGIC;                      --when high will look for a sync pattern if sync not already found, from packet handler
        packet_end_invalid : in  STD_LOGIC;                      --assert to reset synchronisation status                             , from packet handler
        byte_valid         : out STD_LOGIC;                      --goes high as soon as sync pattern is found (so data out on next cycle contains header)
        byte_align_data    : out STD_LOGIC_VECTOR (9 downto 0)
        ); --aligned data out, typically delayed by 2 cycles
	end component csi_rx_byte_align;

	component csi_rx_word_align is
		Port ( word_clock : in STD_LOGIC;                            --byte/word clock in
        reset              : in  STD_LOGIC;                      --active high synchronous reset
        enable             : in  STD_LOGIC;                      --active high enable
        packet_end         : in  STD_LOGIC;                      --packet done input from packet handler entity
        packet_start       : in  STD_LOGIC;                      --whether or not to be looking for an alignment
        packet_end_invalid : out STD_LOGIC;                      --packet done output to byte aligners
        byte_align_data    : in  STD_LOGIC_VECTOR (49 downto 0); --unaligned word from the 4 byte aligners
        byte_valid         : in  STD_LOGIC_VECTOR (4 downto 0);  --word_valid from the byte aligners (MSB is index 3, LSB index 0)
        word_align_data    : out STD_LOGIC_VECTOR (49 downto 0); --aligned word out to packet handler
        word_valid         : out STD_LOGIC
        );                     --goes high once alignment is valid, such that the first word with it high is the CSI packet header											 --aligned data out, typically delayed by 2 cycles
	end component csi_rx_word_align;

	component csi_rx_packet_handler is
		Port ( clock : in STD_LOGIC;                            --word clock in
		reset           : in  STD_LOGIC;                      --asynchronous active high reset
		enable          : in  STD_LOGIC;                      --active high enable
		i_data          : in  STD_LOGIC_VECTOR (49 downto 0); --i_data in from word aligner
		i_data_valid    : in  STD_LOGIC;                      --i_data valid in from word aligner
		packet_start    : out STD_LOGIC;                      --drives byte and word aligner wait_for_sync
		packet_end      : out STD_LOGIC;                      --drives word aligner packet_end
		o_payload       : out STD_LOGIC_VECTOR(39 downto 0);  --payload out from long video packets
		o_payload_valid : out STD_LOGIC;                      --whether or not payload output is valid (i.e. currently receiving a long packet)

		o_frame : out STD_LOGIC; --whether or not currently in video frame (i.e. got FS but not FE)
		o_line  : out STD_LOGIC  --whether or not receiving video line

		);
	end component csi_rx_packet_handler;





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
	signal packet_end           : std_logic;
	signal packet_payload_valid : std_logic;
	signal packet_payload       : std_logic_vector(39 downto 0);
	signal csi_in_frame         : std_logic;
	signal csi_in_line          : std_logic;

	signal word_valid : std_logic;




BEGIN

	inclk_p <= NOT inclk_p after period/2;

	reset          <= '1', '0' after period;
	enable <= '1';


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
			byte_clock_int <= file_byte_clock_int;

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
