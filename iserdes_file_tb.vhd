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



	type test_vector is record
		d0, d1, d2, d3, sync : std_logic;
	end record;

	type test_vector_array is array (natural range <>) of test_vector;

	constant test_vectors : test_vector_array := (
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			-- 3A6 start
			('1', '1', '1', '1', '1'),
			('1', '1', '1', '1', '1'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			-- 3A6 end
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('1', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('1', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('1', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('1', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('1', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),
			('0', '0', '0', '0', '0'),
			('1', '1', '1', '1', '1'),

			('0', '1', '0', '1', '1')
		);


	--Inputs
	--signal dphy_d0   : std_logic_vector(1 downto 0);
	--signal dphy_d1   : std_logic_vector(1 downto 0);
	--signal dphy_d2   : std_logic_vector(1 downto 0);
	--signal dphy_d3   : std_logic_vector(1 downto 0);
	--signal dphy_sync : std_logic_vector(1 downto 0);
	--signal dphy_clk  : std_logic_vector(1 downto 0);

	signal reset   : std_logic;
	signal inclk_p : std_logic := '1' ;
	--signal inclk_n : std_logic;

	signal serdes_reset : std_logic;
	signal deser_data   : std_logic_vector(49 downto 0);

	----------------------------------------------------------------------------
	-- variable for csi_rx_hs_clk_phy
	--signal bit_clock_int_pre : std_logic;
	--signal bit_clock_int     : std_logic;
	--signal bit_clock_b_int   : std_logic;

	----------------------------------------------------------------------------
	-- variable for csi_rx_clock_det
	--signal count_value    : unsigned(3 downto 0); --
	--signal clk_fail       : std_logic;
	--signal ext_clk_lat    : std_logic;
	--signal last_ext_clk   : std_logic;
	--signal clk_fail_count : unsigned(7 downto 0);	

	----------------------------------------------------------------------------
	-- variable for csi_rx_hs_lane_phy
	--signal ddr_bit_clock   : std_logic;
	--signal ddr_bit_clock_b : std_logic;
	signal byte_clock_int : std_logic;

	--signal reset_lat  : std_logic; --reset synchronised to byte clock
	--signal in_se      : std_logic; --input after differential buffer
	--signal in_delayed : std_logic; --input after deskew
	--signal deser_data1 : std_logic_vector(9 downto 0);

	--signal serdes_master_out_int : std_logic_vector(7 downto 0);
	--signal serdes_slave_out_int  : std_logic_vector(7 downto 0);

	--signal SHIFT_TO_SLAVE1 : std_logic;
	--signal SHIFT_TO_SLAVE2 : std_logic;
	--signal SHIFT_FROM_SLAVE1 : std_logic;
	--signal SHIFT_FROM_SLAVE2 : std_logic;


	--attribute dont_touch   : boolean;
	--attribute clock_signal : string;

	--attribute dont_touch of SHIFT_TO_SLAVE1   : signal is true;
	--attribute clock_signal of SHIFT_TO_SLAVE1 : signal is "yes";


	----------------------------------------------------------------------------
	-- csi_rx_byte_align
	signal curr_byte    : std_logic_vector(9 downto 0);
	signal last_byte    : std_logic_vector(9 downto 0);
	signal shifted_byte : std_logic_vector(9 downto 0);

	signal found_hdr      : std_logic;
	signal valid_data_int : std_logic;
	signal hdr_offs       : unsigned(3 downto 0);
	signal data_offs      : unsigned(3 downto 0);

	----------------------------------------------------------------------------
	-- csi_rx_packet_handler
	signal packet_payload            : std_logic_vector(39 downto 0);
	signal packet_payload_valid      : std_logic;
	signal csi_in_frame, csi_in_line : std_logic;

	--------------------------------------------------------------------------

	signal byte_align_data : std_logic_vector(49 downto 0);
	signal byte_valid      : std_logic_vector(4 downto 0);
	signal word_align_data : std_logic_vector(49 downto 0);

	signal byte_packet_done : std_logic;
	signal ref_clock        : std_logic;
	signal enable           : std_logic := '1' ;
	signal wait_for_sync    : std_logic ;
	signal packet_done      : std_logic ;

	signal reset_out  : STD_LOGIC;                      --reset output based on clock detection
	signal word_clock : STD_LOGIC;                      --divided word clock output
	signal word_data  : STD_LOGIC_VECTOR (49 downto 0); --aligned word data output
	signal word_valid : STD_LOGIC;                      --whether or not above data is synced and aligned




	signal byte_clock_out  : std_logic;
	signal o_vsync         : std_logic ;
	signal o_hsync         : std_logic ;
	signal o_4pixels       : std_logic_vector (39 downto 0) ;
	signal o_4pixels_valid : std_logic;

	component csi_rx_hs_clk_phy is

		generic (
			series  : string  := "7SERIES"; --FPGA series, 7SERIES or VIRTEX6
			term_en : boolean := true
		);
		port (
			dphy_clk        : in  STD_LOGIC_VECTOR (1 downto 0); --D-PHY clock input; 1 is P, 0 is N
			reset           : in  STD_LOGIC;                     -- 1 : reset input for BUFR
			ddr_bit_clock   : out STD_LOGIC;                     --DDR bit clock (i.e. input clock buffered) out
			ddr_bit_clock_b : out STD_LOGIC;                     --Inverted DDR bit clock out
			byte_clock      : out STD_LOGIC                      --SDR byte clock (i.e. input clock / 4) out
		);
	end component;


	component csi_rx_clock_det is

		port (
			ref_clock        : in  std_logic; --reference clock in; must not be synchronised to byte_clock and at least 200Mhz
			byte_clock       : in  STD_LOGIC; --
			enable           : in  STD_LOGIC; --active high enable
			reset            : in  STD_LOGIC; --active high asynchronous reset in
			frame_start_tick : out STD_LOGIC
		);
	end component;

	component csi_rx_hs_lane_phy is

		generic(
			series  : string  := "7SERIES"; --FPGA series, 7SERIES or VIRTEX6
			invert  : boolean := false;     --Whether or not to invert output (i.e. if pairs are swapped)
			term_en : boolean := true;      --Whether or not to enable internal input termination
			delay   : natural               --IDELAY delay value for skew compensation
		);

		port (
			ddr_bit_clock   : in  STD_LOGIC; --true and complement DDR bit clocks, buffered from D-PHY clock
			ddr_bit_clock_b : in  STD_LOGIC;
			byte_clock      : in  STD_LOGIC;                     --byte clock; i.e. input clock /4
			enable          : in  STD_LOGIC;                     --active high enable for SERDES
			reset           : in  STD_LOGIC;                     --reset, latched internally to byte clock
			dphy_hs         : in  STD_LOGIC_VECTOR (1 downto 0); --lane input, 1 is P, 0 is N
			deser_out       : out STD_LOGIC_VECTOR (9 downto 0)  --deserialised byte output
		);
	end component;

	component csi_rx_byte_align is

		port (
			clock         : in  STD_LOGIC;                      --byte clock in, word_clock_int, align?´ ?˜ì§? ?žˆì§? ?•Š?‹¤.
			reset         : in  STD_LOGIC;                      --from serdes_reset, synchronous active high reset
			enable        : in  STD_LOGIC;                      --active high enable
			deser_in      : in  STD_LOGIC_VECTOR (9 downto 0);  --raw data from ISERDES
			wait_for_sync : in  STD_LOGIC;                      --when high will look for a sync pattern if sync not already found, from packet handler
			packet_done   : in  STD_LOGIC;                      --assert to reset synchronisation status                             , from packet handler
			valid_data    : out STD_LOGIC;                      --goes high as soon as sync pattern is found (so data out on next cycle contains header)
			data_out      : out STD_LOGIC_VECTOR (9 downto 0)); --aligned data out, typically delayed by 2 cycles
	end component;


	component csi_rx_word_align is

		Port ( word_clock : in STD_LOGIC;                         --byte/word clock in
			reset           : in  STD_LOGIC;                      --active high synchronous reset
			enable          : in  STD_LOGIC;                      --active high enable
			packet_done     : in  STD_LOGIC;                      --packet done input from packet handler entity
			wait_for_sync   : in  STD_LOGIC;                      --whether or not to be looking for an alignment
			packet_done_out : out STD_LOGIC;                      --packet done output to byte aligners
			word_in         : in  STD_LOGIC_VECTOR (49 downto 0); --unaligned word from the 4 byte aligners
			valid_in        : in  STD_LOGIC_VECTOR (4 downto 0);  --valid_out from the byte aligners (MSB is index 3, LSB index 0)
			word_out        : out STD_LOGIC_VECTOR (49 downto 0); --aligned word out to packet handler
			valid_out       : out STD_LOGIC);                     --goes high once alignment is valid, such that the first word with it high is the CSI packet header
	end component;

	component csi_rx_packet_handler is

		port (
			clock 			: in STD_LOGIC;                            --word clock in
			reset           : in  STD_LOGIC;                      --asynchronous active high reset
			enable          : in  STD_LOGIC;                      --active high enable
			i_data          : in  STD_LOGIC_VECTOR (49 downto 0); --i_data in from word aligner
			i_data_valid    : in  STD_LOGIC;                      --i_data valid in from word aligner
			sync_wait       : out STD_LOGIC;                      --drives byte and word aligner wait_for_sync
			packet_done     : out STD_LOGIC;                      --drives word aligner packet_done
			o_payload       : out STD_LOGIC_VECTOR(39 downto 0);  --payload out from long video packets
			o_payload_valid : out STD_LOGIC;                      --whether or not payload output is valid (i.e. currently receiving a long packet)
			o_frame         : out STD_LOGIC;                      --whether or not currently in video frame (i.e. got FS but not FE)
			o_line          : out STD_LOGIC                       --whether or not receiving video line
		);
	end component;



BEGIN

	inclk_p <= NOT inclk_p after period/2;
	-- inclk_n <= not inclk_p;

	--dphy_clk(1) <= inclk_p;
	--dphy_clk(0) <= inclk_n;

	ref_clock <= inclk_p;
	reset     <= '1', '0' after period;

	byte_clock_int <= inclk_p;



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


	--tb1 : process
	--begin
	--	for i in test_vectors'range loop
	--		dphy_d0(1) <= test_vectors(i).d0;
	--		dphy_d0(0) <= not test_vectors(i).d0;

	--		dphy_d1(1) <= test_vectors(i).d1;
	--		dphy_d1(0) <= not test_vectors(i).d1;

	--		dphy_d2(1) <= test_vectors(i).d2;
	--		dphy_d2(0) <= not test_vectors(i).d2;

	--		dphy_d3(1) <= test_vectors(i).d3;
	--		dphy_d3(0) <= not test_vectors(i).d3;

	--		dphy_sync(1) <= test_vectors(i).sync;
	--		dphy_sync(0) <= not test_vectors(i).sync;

	--		wait for period/2;
	--	end loop;
	--	wait;
	--end process;



	----------------------------------------------------------------------------
	----------------------------------------------------------------------------
	--clkphy : csi_rx_hs_clk_phy
	--iclkdbuf : IBUFDS
	--	generic map (
	--		DIFF_TERM    => dphy_term_en,
	--		IBUF_LOW_PWR => FALSE,
	--		IOSTANDARD   => "DEFAULT"
	--	)
	--	port map(
	--		O  => bit_clock_int_pre,
	--		I  => dphy_clk(1),
	--		IB => dphy_clk(0)
	--	);

	--iclkbufio : BUFG
	--	port map (
	--		O => bit_clock_int,
	--		I => bit_clock_int_pre
	--	);


	--clkdiv : BUFR
	--	generic map (
	--		BUFR_DIVIDE => "5",
	--		SIM_DEVICE  => fpga_series
	--	)
	--	port map (
	--		O   => byte_clock_int,
	--		CE  => '1',
	--		CLR => reset,                -- 1 : counter reset
	--		I   => bit_clock_int_pre
	--	);

	--bit_clock_b_int <= NOT bit_clock_int;
	--ddr_bit_clock   <= bit_clock_int;
	--ddr_bit_clock_b <= bit_clock_b_int;

	----------------------------------------------------------------------------

	--clkphy : csi_rx_hs_clk_phy
	--	generic map(
	--		series  => fpga_series,
	--		term_en => dphy_term_en)
	--	port map(
	--		dphy_clk        => dphy_clk,        -- in 
	--		reset           => reset,           -- in 
	--		ddr_bit_clock   => ddr_bit_clock,   -- out
	--		ddr_bit_clock_b => ddr_bit_clock_b, -- out
	--		byte_clock      => byte_clock_int   -- out,
	--	);

	----------------------------------------------------------------------------
	----------------------------------------------------------------------------


	--process(byte_clock_int, reset, clk_fail) --
	--begin
	--	if reset = '1' or clk_fail = '1' then
	--		count_value <= x"0"; -- 
	--	elsif rising_edge(byte_clock_int) then
	--		if enable = '1' then
	--			if count_value < 3 then
	--				count_value <= count_value + 1; -- 
	--			end if;
	--		end if;
	--	end if;
	--end process;

	---- detect the interval between frame , and count the term , save to clk_fail_count
	--process(ref_clock)
	--begin
	--	if rising_edge(ref_clock) then
	--		ext_clk_lat  <= byte_clock_int;
	--		last_ext_clk <= ext_clk_lat;
	--		if last_ext_clk /= ext_clk_lat then    -- 
	--			clk_fail_count <= (others => '0'); -- 
	--		else
	--			if clk_fail_count < 250 then
	--				clk_fail_count <= clk_fail_count + 1; -- 
	--			end if;
	--		end if;
	--	end if;
	--end process;

	--clk_fail         <= '1' when clk_fail_count >= 200 else '0'; -- 
	--serdes_reset <= '1' when count_value < 2 else '0';       -- after reset or clk_fail, 

	----------------------------------------------------------------------------
	-- check if byte_clock is normal,  and if not normal, frame_start_tick <= 1  for 2 clock, and then frame_start_tick <= 0;
	--clkdet : csi_rx_clock_det
	--	port map(
	--		ref_clock        => ref_clock,      -- in
	--		byte_clock       => byte_clock_int, -- in 
	--		enable           => enable,         -- in
	--		reset            => reset,          -- in 
	--		frame_start_tick => serdes_reset    -- out
	--	);


	----------------------------------------------------------------------------
	----------------------------------------------------------------------------
	--process(byte_clock_int)
	--   begin
	--       if rising_edge(byte_clock_int) then
	--           reset_lat <= serdes_reset;
	--       end if;
	--   end process;

	--   inbuf : IBUFDS
	--       generic map(
	--           DIFF_TERM    => dphy_term_en,
	--           IBUF_LOW_PWR => FALSE,
	--           IOSTANDARD   => "DEFAULT")
	--       port map(
	--           O  => in_se,
	--           I  => dphy_d0(1),
	--           IB => dphy_d0(0));



	--   indelay : IDELAYE2
	--       generic map (
	--           CINVCTRL_SEL          => "FALSE",   -- Dynamic clock inversion
	--           DELAY_SRC             => "IDATAIN", -- "I"=IDATAIN, "O"=ODATAIN, "DATAIN"=DATAIN, "IO"=Bi-directional
	--           HIGH_PERFORMANCE_MODE => "TRUE",    -- TRUE specifies lower jitter
	--           IDELAY_TYPE           => "FIXED",   -- "DEFAULT", "FIXED" or "VARIABLE"
	--           IDELAY_VALUE          => d0_skew,     -- 0 to 63 tap values
	--           REFCLK_FREQUENCY      => 200.0,     -- Frequency used for IDELAYCTRL -- 175.0 to 225.0
	--           SIGNAL_PATTERN        => "DATA",    -- Input signal type, "CLOCK" or "DATA"
	--           PIPE_SEL              => "FALSE"
	--       )
	--       port map (
	--           DATAOUT     => in_delayed, -- 1-bit output: Delayed data output
	--           DATAIN      => '0',        -- 1-bit input: Internal delay data input
	--           C           => byte_clock_int, -- 1-bit input: Clock input
	--           CE          => '0',        -- 1-bit input: Active high enable increment/decrement input
	--           INC         => '0',        -- 1-bit input: Increment / Decrement tap delay input
	--           IDATAIN     => in_se,      -- 1-bit input data input (connect to port)
	--           CNTVALUEIN  => "00000",    -- 5-bit input: Counter value input
	--           CNTVALUEOUT => open,       -- 5-bit output: Counter value output
	--           CINVCTRL    => '0',        -- 1-bit input: Dynamic clock inversion input
	--           LD          => '0',        -- 1-bit, In VAR_LOAD mode, it loads the value of CNTVALUEIN.
	--           LDPIPEEN    => '0',        -- 1-bit input: Enable PIPELINE register to load data input
	--           REGRST      => '0'         -- 1-bit input: Active-high reset tap-delay input
	--       );

	--   Master_iserdes : ISERDESE2
	--       generic map (
	--           DATA_RATE         => "DDR",
	--           DATA_WIDTH        => 10,
	--           DYN_CLKDIV_INV_EN => "FALSE", -- Enable DYNCLKDIVINVSEL inversion (FALSE, TRUE)
	--           DYN_CLK_INV_EN    => "FALSE", -- Enable DYNCLKINVSEL inversion (FALSE, TRUE)
	--           INIT_Q1           => '0',
	--           INIT_Q2           => '0',
	--           INIT_Q3           => '0',
	--           INIT_Q4           => '0',
	--           INTERFACE_TYPE    => "NETWORKING",
	--           IOBDELAY          => "IFD",   -- NONE, BOTH, IBUF, IFD
	--           NUM_CE            => 2,       -- Number of clock enables (1,2)
	--           OFB_USED          => "FALSE", -- Select OFB path (FALSE, TRUE)
	--           SERDES_MODE       => "MASTER",
	--           SRVAL_Q1          => '0',
	--           SRVAL_Q2          => '0',
	--           SRVAL_Q3          => '0',
	--           SRVAL_Q4          => '0')
	--       port map (
	--           O            => open, -- 1-bit output: Combinatorial output
	--           Q1           => serdes_master_out_int(0),
	--           Q2           => serdes_master_out_int(1),
	--           Q3           => serdes_master_out_int(2),
	--           Q4           => serdes_master_out_int(3),
	--           Q5           => serdes_master_out_int(4),
	--           Q6           => serdes_master_out_int(5),
	--           Q7           => serdes_master_out_int(6),
	--           Q8           => serdes_master_out_int(7),
	--           SHIFTOUT1    => SHIFT_TO_SLAVE1, -- SHIFTOUT1, SHIFTOUT2: 1-bit (each) output: Data width expansion output ports
	--           SHIFTOUT2    => SHIFT_TO_SLAVE2, -- SHIFTOUT1, SHIFTOUT2: 1-bit (each) output: Data width expansion output ports
	--           BITSLIP      => '0',             -- 1-bit input: The BITSLIP pin performs a Bitslip operation synchronous to CLKDIV when asserted (active High)
	--           CE1          => '1',             -- CE1, CE2: 1-bit (each) input: Data register clock enable inputs
	--           CE2          => '1',             -- CE1, CE2: 1-bit (each) input: Data register clock enable inputs
	--           CLKDIVP      => '0',             -- 1-bit input: TBD
	--           CLK          => ddr_bit_clock,   -- 1-bit input: High-speed clock
	--           CLKB         => ddr_bit_clock_b, -- 1-bit input: High-speed secondary clock
	--           CLKDIV       => byte_clock_int,      -- 1-bit input: Divided clock
	--           OCLK         => '0',             -- 1-bit input: High speed output clock used when INTERFACE_TYPE="MEMORY"
	--           DYNCLKDIVSEL => '0',             -- 1-bit input: Dynamic CLKDIV inversion
	--           DYNCLKSEL    => '0',             -- 1-bit input: Dynamic CLK/CLKB inversion
	--           D            => '0',             -- 1-bit input: Data input
	--           DDLY         => in_delayed,      -- 1-bit input: Serial data from IDELAYE2
	--           OFB          => '0',             -- 1-bit input: Data feedback from OSERDESE2
	--           OCLKB        => '0',             -- 1-bit input: High speed negative edge output clock -- Dynamic Clock Inversions: 1-bit (each) input: Dynamic clock inv pins to switch clk polarity
	--           RST          => reset_lat,       -- 1-bit input: High speed output clock used when INTERFACE_TYPE="MEMORY"
	--           SHIFTIN1     => '0',             -- SHIFTIN1, SHIFTIN2: 1-bit (each) input: Data width expansion input ports
	--           SHIFTIN2     => '0'              -- SHIFTIN1, SHIFTIN2: 1-bit (each) input: Data width expansion input ports
	--       );

	--   Slave_iserdes : ISERDESE2
	--       generic map (
	--           DATA_RATE         => "DDR",
	--           DATA_WIDTH        => 10,
	--           DYN_CLKDIV_INV_EN => "FALSE", -- Enable DYNCLKDIVINVSEL inversion (FALSE, TRUE)
	--           DYN_CLK_INV_EN    => "FALSE", -- Enable DYNCLKINVSEL inversion (FALSE, TRUE)
	--           INIT_Q1           => '0',
	--           INIT_Q2           => '0',
	--           INIT_Q3           => '0',
	--           INIT_Q4           => '0',
	--           INTERFACE_TYPE    => "NETWORKING",
	--           IOBDELAY          => "NONE",  -- NONE, BOTH, IBUF, IFD
	--           NUM_CE            => 2,       -- Number of clock enables (1,2)
	--           OFB_USED          => "FALSE", -- Select OFB path (FALSE, TRUE)
	--           SERDES_MODE       => "SLAVE",
	--           SRVAL_Q1          => '0',
	--           SRVAL_Q2          => '0',
	--           SRVAL_Q3          => '0',
	--           SRVAL_Q4          => '0')
	--       port map (
	--           O            => open, -- 1-bit output: Combinatorial output
	--           Q1           => serdes_slave_out_int(0),
	--           Q2           => serdes_slave_out_int(1),
	--           Q3           => serdes_slave_out_int(2),
	--           Q4           => serdes_slave_out_int(3),
	--           Q5           => serdes_slave_out_int(4),
	--           Q6           => serdes_slave_out_int(5),
	--           Q7           => serdes_slave_out_int(6),
	--           Q8           => serdes_slave_out_int(7),
	--           SHIFTOUT1    => SHIFT_FROM_SLAVE1, -- SHIFTOUT1, SHIFTOUT2: 1-bit (each) output: Data width expansion output ports
	--           SHIFTOUT2    => SHIFT_FROM_SLAVE2, -- SHIFTOUT1, SHIFTOUT2: 1-bit (each) output: Data width expansion output ports
	--           BITSLIP      => '0',               -- 1-bit input: The BITSLIP pin performs a Bitslip operation synchronous to CLKDIV when asserted (active High)
	--           CE1          => '1',               -- CE1, CE2: 1-bit (each) input: Data register clock enable inputs
	--           CE2          => '1',               -- CE1, CE2: 1-bit (each) input: Data register clock enable inputs
	--           CLKDIVP      => '0',               -- 1-bit input: TBD
	--           CLK          => ddr_bit_clock,     -- 1-bit input: High-speed clock
	--           CLKB         => ddr_bit_clock_b,   -- 1-bit input: High-speed secondary clock
	--           CLKDIV       => byte_clock_int,        -- 1-bit input: Divided clock
	--           OCLK         => '0',               -- 1-bit input: High speed output clock used when INTERFACE_TYPE="MEMORY"
	--           DYNCLKDIVSEL => '0',               -- 1-bit input: Dynamic CLKDIV inversion
	--           DYNCLKSEL    => '0',               -- 1-bit input: Dynamic CLK/CLKB inversion
	--           D            => '0',               -- 1-bit input: Data input
	--           DDLY         => '0',               -- 1-bit input: Serial data from IDELAYE2
	--           OFB          => '0',               -- 1-bit input: Data feedback from OSERDESE2
	--           OCLKB        => '0',               -- 1-bit input: High speed negative edge output clock -- Dynamic Clock Inversions: 1-bit (each) input: Dynamic clock inv pins to switch clk polarity
	--           RST          => reset_lat,         -- 1-bit input: High speed output clock used when INTERFACE_TYPE="MEMORY"
	--           SHIFTIN1     => SHIFT_TO_SLAVE1,   -- SHIFTIN1, SHIFTIN2: 1-bit (each) input: Data width expansion input ports
	--           SHIFTIN2     => SHIFT_TO_SLAVE2    -- SHIFTIN1, SHIFTIN2: 1-bit (each) input: Data width expansion input ports
	--       );



	--   deser_data1(7 downto 0) <= serdes_master_out_int(7 downto 0);
	--   deser_data1(9 downto 8) <= serdes_slave_out_int(3 downto 2);

	--   deser_data(7 downto 0) <= serdes_master_out_int(7 downto 0);
	--   deser_data(9 downto 8) <= serdes_slave_out_int(3 downto 2);



	----------------------------------------------------------------------------
	--d0phy : csi_rx_hs_lane_phy
	--	generic map(
	--		series  => fpga_series,
	--		invert  => d0_invert,
	--		term_en => dphy_term_en,
	--		delay   => d0_skew)
	--	port map (
	--		ddr_bit_clock   => ddr_bit_clock,         -- in
	--		ddr_bit_clock_b => ddr_bit_clock_b,       -- in
	--		byte_clock      => byte_clock_int,        -- in
	--		enable          => enable,                -- in
	--		reset           => serdes_reset,          -- in
	--		dphy_hs         => dphy_d0,               -- in
	--		deser_out       => deser_data(9 downto 0) -- out
	--	);

	----------------------------------------------------------------------------
	----------------------------------------------------------------------------

	--d1phy : csi_rx_hs_lane_phy
	--	generic map(
	--		series  => fpga_series,
	--		invert  => d1_invert,
	--		term_en => dphy_term_en,
	--		delay   => d1_skew)
	--	port map (
	--		ddr_bit_clock   => ddr_bit_clock,
	--		ddr_bit_clock_b => ddr_bit_clock_b,
	--		byte_clock      => byte_clock_int,
	--		enable          => enable,
	--		reset           => serdes_reset,
	--		dphy_hs         => dphy_d1,
	--		deser_out       => deser_data(19 downto 10));

	--d2phy : csi_rx_hs_lane_phy
	--	generic map(
	--		series  => fpga_series,
	--		invert  => d2_invert,
	--		term_en => dphy_term_en,
	--		delay   => d2_skew)
	--	port map (
	--		ddr_bit_clock   => ddr_bit_clock,
	--		ddr_bit_clock_b => ddr_bit_clock_b,
	--		byte_clock      => byte_clock_int,
	--		enable          => enable,
	--		reset           => serdes_reset,
	--		dphy_hs         => dphy_d2,
	--		deser_out       => deser_data(29 downto 20));

	--d3phy : csi_rx_hs_lane_phy
	--	generic map(
	--		series  => fpga_series,
	--		invert  => d3_invert,
	--		term_en => dphy_term_en,
	--		delay   => d3_skew)
	--	port map (
	--		ddr_bit_clock   => ddr_bit_clock,
	--		ddr_bit_clock_b => ddr_bit_clock_b,
	--		byte_clock      => byte_clock_int,
	--		enable          => enable,
	--		reset           => serdes_reset,
	--		dphy_hs         => dphy_d3,
	--		deser_out       => deser_data(39 downto 30));

	--syncphy : csi_rx_hs_lane_phy
	--	generic map(
	--		series  => fpga_series,
	--		invert  => d3_invert,
	--		term_en => dphy_term_en,
	--		delay   => d3_skew)
	--	port map (
	--		ddr_bit_clock   => ddr_bit_clock,
	--		ddr_bit_clock_b => ddr_bit_clock_b,
	--		byte_clock      => byte_clock_int,
	--		enable          => enable,
	--		reset           => serdes_reset,
	--		dphy_hs         => dphy_sync,
	--		deser_out       => deser_data(49 downto 40));


	----------------------------------------------------------------------------
	----------------------------------------------------------------------------

	process(byte_clock_int)
	begin
		if rising_edge(byte_clock_int) then
			if serdes_reset = '1' then
				valid_data_int <= '0';
			elsif enable = '1' then
				last_byte                    <= curr_byte;
				curr_byte                    <= deser_data(9 downto 0 );
				byte_align_data(9 downto 0 ) <= shifted_byte;

				if byte_packet_done = '1' then
					valid_data_int <= found_hdr;
				elsif wait_for_sync = '1' and found_hdr = '1' and valid_data_int = '0' then
					valid_data_int <= '1';
					data_offs      <= hdr_offs;
				end if;
			end if;
		end if;
	end process;

	byte_valid <= '0' & '0' & '0' & '0' & valid_data_int;
	--This assumes that data is arranged correctly (chronologically last bit in MSB)
	--and looks for the "10111000" sync sequence


	process(curr_byte, last_byte)
		constant sync      : std_logic_vector(9 downto 0) := "1110100110";
		variable was_found : boolean                      := false;
		variable offset    : integer range 0 to 9;
	begin
		offset    := 0;
		was_found := false;
		for i in 0 to 9 loop
			if (last_byte(i-1 downto 0) & curr_byte(9 downto i ) = sync) then
				was_found := true;
				offset    := i;
			end if;
		end loop;
		if was_found then
			found_hdr <= '1';
			hdr_offs  <= to_unsigned(offset, 4);
		else
			found_hdr <= '0';
			hdr_offs  <= "0000";
		end if;
	end process;

	--This aligns the data correctly

	shifted_byte <= curr_byte(9 downto 0 )          when data_offs = 0 else
		last_byte(0 downto 0) & curr_byte(9 downto 1 ) when data_offs = 1 else
		last_byte(1 downto 0) & curr_byte(9 downto 2 ) when data_offs = 2 else
		last_byte(2 downto 0) & curr_byte(9 downto 3 ) when data_offs = 3 else
		last_byte(3 downto 0) & curr_byte(9 downto 4 ) when data_offs = 4 else
		last_byte(4 downto 0) & curr_byte(9 downto 5 ) when data_offs = 5 else
		last_byte(5 downto 0) & curr_byte(9 downto 6 ) when data_offs = 6 else
		last_byte(6 downto 0) & curr_byte(9 downto 7 ) when data_offs = 7 else
		last_byte(7 downto 0) & curr_byte(9 downto 8 ) when data_offs = 8 else
		last_byte(8 downto 0) & curr_byte(9 downto 9 );

	----------------------------------------------------------------------------    

	--gen_bytealign : for i in 0 to 4 generate
	--	ba : csi_rx_byte_align
	--		port map (
	--			clock         => byte_clock_int,                           -- in 
	--			reset         => serdes_reset,                             -- in 
	--			enable        => enable,                                   -- in 
	--			deser_in      => deser_data((10*i) + 9 downto 10 * i),     -- in 
	--			wait_for_sync => wait_for_sync,                            -- in 
	--			packet_done   => byte_packet_done,                         -- in 
	--			valid_data    => byte_valid(i),                            -- out
	--			data_out      => byte_align_data((10*i) + 9 downto 10 * i) -- out
	--		);
	--end generate;

	----------------------------------------------------------------------------
	----------------------------------------------------------------------------

	wordalign : csi_rx_word_align
		port map (
			word_clock      => byte_clock_int,   -- in 
			reset           => serdes_reset,     -- in 
			enable          => enable,           -- in 
			packet_done     => packet_done,      -- in 
			wait_for_sync   => wait_for_sync,    -- in 
			packet_done_out => byte_packet_done, -- out
			word_in         => byte_align_data,  -- in 
			valid_in        => byte_valid,       -- in 
			word_out        => word_align_data,  -- out
			valid_out       => word_valid);      -- out

	word_clock <= byte_clock_int;
	word_data  <= word_align_data;
	reset_out  <= serdes_reset;


	depacket : csi_rx_packet_handler
		port map (
			clock           => byte_clock_int,       -- in 
			reset           => serdes_reset,         -- in 
			enable          => enable,               -- in 
			i_data          => word_align_data,      -- in 
			i_data_valid    => word_valid,           -- in 
			sync_wait       => wait_for_sync,        -- out
			packet_done     => packet_done,          -- out
			o_payload       => packet_payload,       -- out
			o_payload_valid => packet_payload_valid, -- out
			o_frame         => csi_in_frame,         -- out
			o_line          => csi_in_line           -- out
		);







END behavior;
