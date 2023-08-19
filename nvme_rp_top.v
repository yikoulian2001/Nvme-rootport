`resetall
`timescale  1ns/1ps

`include "top_param.vh"        //for synthesis

module  nvme_rp_top #(
parameter   PCIE_ID =   0
)
(
`ifdef pcie_core_rp
//pcie rp
input                   pcie_sys_clk_p      ,
input                   pcie_sys_clk_n      ,
output                  pcie_resetn         ,
output      [ 3: 0]     pcie_txp            ,
output      [ 3: 0]     pcie_txn            ,
input       [ 3: 0]     pcie_rxp            ,
input       [ 3: 0]     pcie_rxn            ,
`endif
input                   cbus_clk            ,
input                   cbus_rst            ,
input                   user_clk            ,
input                   user_rst            ,

//input       [35: 0]     curr_time           ,
//input       [ 8: 0]     cap_radrs           ,
input                   rp_reset            ,
output                  rp_init_done        ,
input                   cbus_req            ,
input                   cbus_rw             ,
input       [12: 0]     cbus_addr           ,
input       [31: 0]     cbus_wdata          ,
output                  cbus_ack            ,
output      [31: 0]     cbus_rdata          ,

input       [15: 0]     chan_rx_data_ex     ,
input       [127:0]     chan_rx_data        ,
input                   chan_rx_wen         ,
output                  chan_rx_ready       ,

output      [15: 0]     chan_tx_data_ex     ,
output      [127:0]     chan_tx_data        ,
output                  chan_tx_wen         ,
input                   chan_tx_ready

);


//============root port================
//rp x4
wire                    pcie_clk                ;
wire                    pcie_rst                ;
wire                    pcie_lnk_up             ;


wire    [127:0]         s_axis_rq_tdata         ;
wire    [61: 0]         s_axis_rq_tuser         ;
wire    [ 3: 0]         s_axis_rq_tkeep         ;
wire                    s_axis_rq_tlast         ;
wire                    s_axis_rq_tvalid        ;
wire    [ 3: 0]         s_axis_rq_tready        ;

wire    [127:0]         m_axis_rc_tdata         ;
wire    [74: 0]         m_axis_rc_tuser         ;
wire    [ 3: 0]         m_axis_rc_tkeep         ;
wire                    m_axis_rc_tlast         ;
wire                    m_axis_rc_tvalid        ;
wire                    m_axis_rc_tready        ;

wire    [127:0]         m_axis_cq_tdata         ;
wire    [87: 0]         m_axis_cq_tuser         ;
wire    [ 3: 0]         m_axis_cq_tkeep         ;
wire                    m_axis_cq_tlast         ;
wire                    m_axis_cq_tvalid        ;
wire                    m_axis_cq_tready        ;

wire    [127:0]         s_axis_cc_tdata         ;
wire    [32: 0]         s_axis_cc_tuser         ;
wire    [ 3: 0]         s_axis_cc_tkeep         ;
wire                    s_axis_cc_tlast         ;
wire                    s_axis_cc_tvalid        ;
wire    [ 3: 0]         s_axis_cc_tready        ;

wire    [ 9: 0]         cfg_mgmt_addr           ;
wire                    cfg_mgmt_write          ;
wire    [31: 0]         cfg_mgmt_write_data     ;
wire    [ 3: 0]         cfg_mgmt_byte_enable    ;
wire                    cfg_mgmt_read           ;
wire    [31: 0]         cfg_mgmt_read_data      ;
wire                    cfg_mgmt_read_write_done;

wire    [63: 0]         cfg_status_dbg          ;
wire    [ 7: 0]         ds_bus_number           ;
wire                    pcie_manual_rst         ;
wire    [31: 0]         set_ssd_version         ;
wire    [ 5: 0]         exec_hold_ctl           ;
wire    [31: 0]         exec_hold_time          ;
wire    [ 9: 0]         cq_set_tout             ;
wire    [ 9: 0]         sq_set_tout             ;




wire    [15: 0]         raw_data_ex         ;
wire    [127:0]         raw_data            ;
wire                    raw_wen             ;
wire                    raw_ready           ;

wire    [15: 0]         rq_oper_data_ex     ;
wire    [127:0]         rq_oper_data        ;
wire                    rq_oper_wen         ;
wire                    rq_oper_ready       ;

wire    [15: 0]         rc_cplr_data_ex     ;
wire    [127:0]         rc_cplr_data        ;
wire                    rc_cplr_wen         ;
wire                    rc_cplr_ready       ;

wire    [15: 0]         cq_oper_data_ex     ;
wire    [127:0]         cq_oper_data        ;
wire                    cq_oper_wen         ;
wire                    cq_oper_ready       ;

wire    [15: 0]         cc_cplr_data_ex     ;
wire    [127:0]         cc_cplr_data        ;
wire                    cc_cplr_wen         ;
wire                    cc_cplr_ready       ;


wire    [15: 0]         cq_user_data_ex     ;
wire    [127:0]         cq_user_data        ;
wire                    cq_user_wen         ;
wire                    cq_user_ready       ;

wire    [15: 0]         cc_user_data_ex     ;
wire    [127:0]         cc_user_data        ;
wire                    cc_user_wen         ;
wire                    cc_user_ready       ;

wire    [15: 0]         cq_local_data_ex    ;
wire    [127:0]         cq_local_data       ;
wire                    cq_local_wen        ;
wire                    cq_local_ready      ;

//wire    [15: 0]         cc_local_data_ex    ;
//wire    [127:0]         cc_local_data       ;
//wire                    cc_local_wen        ;
//wire                    cc_local_ready      ;
//
//wire    [15: 0]         cc_trans_data_ex    ;
//wire    [127:0]         cc_trans_data       ;
//wire                    cc_trans_wen        ;
//wire                    cc_trans_ready      ;

wire    [15: 0]         cq_msg_data_ex      ;
wire    [127:0]         cq_msg_data         ;
wire                    cq_msg_wen          ;
wire                    cq_msg_ready        ;

wire    [15: 0]         cq_trans_data_ex    ;
wire    [127:0]         cq_trans_data       ;
wire                    cq_trans_wen        ;
wire                    cq_trans_ready      ;

wire    [15: 0]         cc_trans_data_ex    ;
wire    [127:0]         cc_trans_data       ;
wire                    cc_trans_wen        ;
wire                    cc_trans_ready      ;

//wire    [15: 0]         read_rid            ;
wire    [103:0]         read_info           ;
wire                    read_req            ;

wire                    dfx_cbus_req        ;
wire                    dfx_cbus_rw         ;
wire    [ 9: 0]         dfx_cbus_addr       ;
wire    [31: 0]         dfx_cbus_wdata      ;
wire                    dfx_cbus_ack        ;
wire    [31: 0]         dfx_cbus_rdata      ;

wire                    dbg_lbus_req        ;
wire    [ 3: 0]         dbg_lbus_type       ;
wire    [15: 0]         dbg_lbus_addr       ;
wire    [31: 0]         dbg_lbus_wdata      ;
wire                    dbg_lbus_ack        ;
wire    [31: 0]         dbg_lbus_rdata      ;
//wire    [ 3: 0]         cbus_req            ;
////wire    [ 2: 0]         cbus_fn             ;
////wire                    cbus_rw             ;
////wire    [15: 0]         cbus_addr           ;
////wire    [31: 0]         cbus_wdata          ;
//wire    [ 3: 0]         cbus_ack            ;
//wire    [31: 0]         cbus_0_rdata        ;
//wire    [31: 0]         cbus_1_rdata        ;
//wire    [31: 0]         cbus_2_rdata        ;
//wire    [31: 0]         cbus_3_rdata        ;
//
//wire                    nvme_cbus_req       ;
//wire                    nvme_cbus_ack       ;
//wire    [31: 0]         nvme_cbus_rdata     ;
//wire                    host_cbus_req       ;
//wire                    host_cbus_ack       ;
//wire    [31: 0]         host_cbus_rdata     ;
//
//wire    [33: 0]         cfg_tx_data         ;
//wire                    cfg_tx_wen          ;
//wire    [55: 0]         cfg_rx_data         ;
//wire                    cfg_rx_wen          ;

wire    [ 7: 0]         pcie_user_if_inc    ;
wire    [63: 0]         pcie_user_if_dbg    ;
wire    [31: 0]         pcie_usr_cq_dbg     ;
wire    [31: 0]         pcie_usr_cc_dbg     ;

wire    [ 1: 0]         dev_onsite          ;
wire                    ssd_err             ;
wire                    init_done           ;
wire    [23: 0]         class_code          ;
wire    [31: 0]         ssd_version         ;
wire    [15: 0]         ssd_qsize           ;

wire    [31: 0]         ssd_sq_tail         ;
wire    [31: 0]         ssd_sq_head         ;
wire    [31: 0]         ssd_cq_head         ;
wire    [63: 0]         admin_ctl_dbg       ;
wire    [31: 0]         io_ctl_rp_dbg       ;
wire    [31: 0]         sqmsg_op_dbg        ;
wire    [31: 0]         sq_ctl_rp_dbg       ;
wire    [31: 0]         cq_ctl_rp_dbg       ;
wire    [31: 0]         pcie_cq_ingress_dbg ;
wire    [31: 0]         pcie_cc_egress_dbg  ;
wire    [31: 0]         rp_cq_order_dbg     ;
wire    [31: 0]         rp_cc_combine_dbg   ;
wire    [31: 0]         stream_mux_rp_dbg   ;
wire    [ 5: 0]         io_ctl_rp_inc       ;
wire    [ 1: 0]         sqmsg_op_inc        ;
wire    [ 4: 0]         sq_ctl_rp_inc       ;
wire    [ 5: 0]         cq_ctl_rp_inc       ;
wire    [ 3: 0]         pcie_cq_ingress_inc ;
wire    [ 1: 0]         pcie_cc_egress_inc  ;
wire    [ 6: 0]         rp_cq_order_inc     ;
wire    [ 1: 0]         rp_cc_combine_inc   ;
wire    [ 1: 0]         stream_mux_rp_inc   ;


//reg     [35: 0]         curr_time_meta  ;
//reg     [35: 0]         curr_time_syn   ;
//always@(posedge pcie_clk)
//begin
//    curr_time_meta <= curr_time;
//    curr_time_syn  <= curr_time_meta;
//end
assign rp_init_done = init_done;

//===================================== Pci Express Root Port ====================================//
(* ASYNC_REG = "TRUE" *)reg         rp_reset_meta  ;
(* ASYNC_REG = "TRUE" *)reg         rp_reset_syn   ;
always@(posedge cbus_clk)
begin
    rp_reset_meta <= rp_reset;
    rp_reset_syn  <= rp_reset_meta;
end

(* keep="true" *)reg    pcie_resetn_c;
always@(posedge cbus_clk)
begin
    if(cbus_rst)
        pcie_resetn_c <= 1'b0;
//    else if(rp_reset_syn)
//        pcie_resetn_c <= 1'b0;
    else if(pcie_manual_rst)
        pcie_resetn_c <= 1'b0;
    else
        pcie_resetn_c <= 1'b1;
end


`ifdef  pcie_core_rp
OBUF rp_sys_reset_n_obuf (.O(pcie_resetn), .I(pcie_resetn_c));
pcie_top_rp #(
    .PCIE_ID    ( PCIE_ID   )
)
u_pcie_top_rp(
    .pci_exp_txp        ( pcie_txp          ),
    .pci_exp_txn        ( pcie_txn          ),
    .pci_exp_rxp        ( pcie_rxp          ),
    .pci_exp_rxn        ( pcie_rxn          ),
    .sys_clk_p          ( pcie_sys_clk_p    ),
    .sys_clk_n          ( pcie_sys_clk_n    ),
    .sys_rst_n          ( pcie_resetn_c     ),

    .user_clk           ( pcie_clk          ),
    .user_reset         ( pcie_rst          ),
    .user_lnk_up        ( pcie_lnk_up       ),

    .s_axis_rq_tdata    ( s_axis_rq_tdata   ),
    .s_axis_rq_tuser    ( s_axis_rq_tuser   ),
    .s_axis_rq_tkeep    ( s_axis_rq_tkeep   ),
    .s_axis_rq_tlast    ( s_axis_rq_tlast   ),
    .s_axis_rq_tvalid   ( s_axis_rq_tvalid  ),
    .s_axis_rq_tready   ( s_axis_rq_tready  ),

    .m_axis_rc_tdata    ( m_axis_rc_tdata   ),
    .m_axis_rc_tuser    ( m_axis_rc_tuser   ),
    .m_axis_rc_tkeep    ( m_axis_rc_tkeep   ),
    .m_axis_rc_tlast    ( m_axis_rc_tlast   ),
    .m_axis_rc_tvalid   ( m_axis_rc_tvalid  ),
    .m_axis_rc_tready   ( m_axis_rc_tready  ),

    .m_axis_cq_tdata    ( m_axis_cq_tdata   ),
    .m_axis_cq_tuser    ( m_axis_cq_tuser   ),
    .m_axis_cq_tkeep    ( m_axis_cq_tkeep   ),
    .m_axis_cq_tlast    ( m_axis_cq_tlast   ),
    .m_axis_cq_tvalid   ( m_axis_cq_tvalid  ),
    .m_axis_cq_tready   ( m_axis_cq_tready  ),

    .s_axis_cc_tdata    ( s_axis_cc_tdata   ),
    .s_axis_cc_tuser    ( s_axis_cc_tuser   ),
    .s_axis_cc_tkeep    ( s_axis_cc_tkeep   ),
    .s_axis_cc_tlast    ( s_axis_cc_tlast   ),
    .s_axis_cc_tvalid   ( s_axis_cc_tvalid  ),
    .s_axis_cc_tready   ( s_axis_cc_tready  ),

    .cfg_mgmt_addr              ( cfg_mgmt_addr           ),
    .cfg_mgmt_write             ( cfg_mgmt_write          ),
    .cfg_mgmt_write_data        ( cfg_mgmt_write_data     ),
    .cfg_mgmt_byte_enable       ( cfg_mgmt_byte_enable    ),
    .cfg_mgmt_read              ( cfg_mgmt_read           ),
    .cfg_mgmt_read_data         ( cfg_mgmt_read_data      ),
    .cfg_mgmt_read_write_done   ( cfg_mgmt_read_write_done),

    .cfg_status_dbg     ( cfg_status_dbg    )

);
assign ds_bus_number = cfg_status_dbg[31:24];
`else
assign ds_bus_number = 8'h45;
`endif

pcie_user_if_rp #(
    .DWIDTH     ( 128   )
)
u_pcie_user_if_rp(
    .pcie_clk           ( pcie_clk          ),
    .pcie_rst           ( pcie_rst          ),
    .pcie_link_up       ( pcie_lnk_up       ),

    .s_axis_rq_tdata    ( s_axis_rq_tdata   ),
    .s_axis_rq_tuser    ( s_axis_rq_tuser   ),
    .s_axis_rq_tkeep    ( s_axis_rq_tkeep   ),
    .s_axis_rq_tlast    ( s_axis_rq_tlast   ),
    .s_axis_rq_tvalid   ( s_axis_rq_tvalid  ),
    .s_axis_rq_tready   ( s_axis_rq_tready  ),

    .m_axis_rc_tdata    ( m_axis_rc_tdata   ),
    .m_axis_rc_tuser    ( m_axis_rc_tuser   ),
    .m_axis_rc_tkeep    ( m_axis_rc_tkeep   ),
    .m_axis_rc_tlast    ( m_axis_rc_tlast   ),
    .m_axis_rc_tvalid   ( m_axis_rc_tvalid  ),
    .m_axis_rc_tready   ( m_axis_rc_tready  ),

    .m_axis_cq_tdata    ( m_axis_cq_tdata   ),
    .m_axis_cq_tuser    ( m_axis_cq_tuser   ),
    .m_axis_cq_tkeep    ( m_axis_cq_tkeep   ),
    .m_axis_cq_tlast    ( m_axis_cq_tlast   ),
    .m_axis_cq_tvalid   ( m_axis_cq_tvalid  ),
    .m_axis_cq_tready   ( m_axis_cq_tready  ),

    .s_axis_cc_tdata    ( s_axis_cc_tdata   ),
    .s_axis_cc_tuser    ( s_axis_cc_tuser   ),
    .s_axis_cc_tkeep    ( s_axis_cc_tkeep   ),
    .s_axis_cc_tlast    ( s_axis_cc_tlast   ),
    .s_axis_cc_tvalid   ( s_axis_cc_tvalid  ),
    .s_axis_cc_tready   ( s_axis_cc_tready  ),

    .rq_oper_data_ex    ( rq_oper_data_ex   ),
    .rq_oper_data       ( rq_oper_data      ),
    .rq_oper_wen        ( rq_oper_wen       ),
    .rq_oper_ready      ( rq_oper_ready     ),

    .rc_cplr_data_ex    ( rc_cplr_data_ex   ),
    .rc_cplr_data       ( rc_cplr_data      ),
    .rc_cplr_wen        ( rc_cplr_wen       ),
    .rc_cplr_ready      ( rc_cplr_ready     ),

    .cq_oper_data_ex    ( cq_oper_data_ex   ),
    .cq_oper_data       ( cq_oper_data      ),
    .cq_oper_wen        ( cq_oper_wen       ),
    .cq_oper_ready      ( cq_oper_ready     ),

    .cc_cplr_data_ex    ( cc_cplr_data_ex   ),
    .cc_cplr_data       ( cc_cplr_data      ),
    .cc_cplr_wen        ( cc_cplr_wen       ),
    .cc_cplr_ready      ( cc_cplr_ready     ),

//    .cbus_clk           ( cbus_clk          ),
//    .curr_time          ( curr_time_syn     ),
//    .cap_radrs          ( cap_radrs         ),

    .odbg_inc           ( pcie_user_if_inc  ),
    .odbg_info          ( pcie_user_if_dbg  )
);

pcie_usr_top_rp u_pcie_usr_top_rp(
    .pcie_clk           ( pcie_clk          ),
    .pcie_rst           ( pcie_rst          ),

    .cq_oper_data_ex    ( cq_oper_data_ex   ),
    .cq_oper_data       ( cq_oper_data      ),
    .cq_oper_wen        ( cq_oper_wen       ),
    .cq_oper_ready      ( cq_oper_ready     ),

    .cc_cplr_data_ex    ( cc_cplr_data_ex   ),
    .cc_cplr_data       ( cc_cplr_data      ),
    .cc_cplr_wen        ( cc_cplr_wen       ),
    .cc_cplr_ready      ( cc_cplr_ready     ),

    .cq_user_data_ex    ( cq_user_data_ex   ),
    .cq_user_data       ( cq_user_data      ),
    .cq_user_wen        ( cq_user_wen       ),
    .cq_user_ready      ( cq_user_ready     ),

    .cc_user_data_ex    ( cc_user_data_ex   ),
    .cc_user_data       ( cc_user_data      ),
    .cc_user_wen        ( cc_user_wen       ),
    .cc_user_ready      ( cc_user_ready     ),

    .pcie_usr_cq_dbg    ( pcie_usr_cq_dbg   ),
    .pcie_usr_cc_dbg    ( pcie_usr_cc_dbg   )
);
//pcie_usr_cc #(
//    .RCB    ( 64    )          //option 64/128
//)
//u_pcie_usr_cc(
//    .user_clk           ( pcie_clk          ),
//    .user_rst           ( pcie_rst          ),
//    .cc_oper_data_ex    ( cc_oper_data_ex   ),
//    .cc_oper_data       ( cc_oper_data      ),
//    .cc_oper_wen        ( cc_oper_wen       ),
//    .cc_oper_ready      ( cc_oper_ready     ),
//
//    .cc_cplr_data_ex    ( cc_cplr_data_ex   ),
//    .cc_cplr_data       ( cc_cplr_data      ),
//    .cc_cplr_wen        ( cc_cplr_wen       ),
//    .cc_cplr_ready      ( cc_cplr_ready     ),
//
//    .ostatus_dbg        ( pcie_usr_cc_dbg   )
//);


dbg_lbus_ctl u_dbg_lbus_ctl(
    .cbus_clk               ( cbus_clk      ),
    .cbus_rst               ( cbus_rst      ),

    .cbus_req               ( cbus_req      ),
    .cbus_rw                ( cbus_rw       ),
    .cbus_addr              ( cbus_addr     ),
    .cbus_wdata             ( cbus_wdata    ),
    .cbus_ack               ( cbus_ack      ),
    .cbus_rdata             ( cbus_rdata    ),

    .dfx_cbus_req           ( dfx_cbus_req  ),
    .dfx_cbus_rw            ( dfx_cbus_rw   ),
    .dfx_cbus_addr          ( dfx_cbus_addr ),
    .dfx_cbus_wdata         ( dfx_cbus_wdata),
    .dfx_cbus_ack           ( dfx_cbus_ack  ),
    .dfx_cbus_rdata         ( dfx_cbus_rdata),

    .pcie_clk               ( pcie_clk      ),
    .pcie_rst               ( pcie_rst      ),

    .dbg_lbus_req           ( dbg_lbus_req  ),
    .dbg_lbus_type          ( dbg_lbus_type ),
    .dbg_lbus_addr          ( dbg_lbus_addr ),
    .dbg_lbus_wdata         ( dbg_lbus_wdata),
    .dbg_lbus_ack           ( dbg_lbus_ack  ),
    .dbg_lbus_rdata         ( dbg_lbus_rdata)
);


rp_cq_order u_rp_cq_order(
    .pcie_clk           ( pcie_clk          ),
    .pcie_rst           ( pcie_rst          ),
    .user_clk           ( user_clk          ),
    .user_rst           ( user_rst          ),
    .cq_mix_data_ex     ( cq_user_data_ex   ),
    .cq_mix_data        ( cq_user_data      ),
    .cq_mix_wen         ( cq_user_wen       ),
    .cq_mix_ready       ( cq_user_ready     ),

    .cq_local_data_ex   ( cq_local_data_ex  ),
    .cq_local_data      ( cq_local_data     ),
    .cq_local_wen       ( cq_local_wen      ),
    .cq_local_ready     ( cq_local_ready    ),

    .cq_msg_data_ex     ( cq_msg_data_ex    ),
    .cq_msg_data        ( cq_msg_data       ),
    .cq_msg_wen         ( cq_msg_wen        ),
    .cq_msg_ready       ( cq_msg_ready      ),

    .cq_trans_data_ex   ( cq_trans_data_ex  ),
    .cq_trans_data      ( cq_trans_data     ),
    .cq_trans_wen       ( cq_trans_wen      ),
    .cq_trans_ready     ( cq_trans_ready    ),

//    .read_rid           ( read_rid          ),
    .read_info          ( read_info         ),
    .read_req           ( read_req          ),
    .ostat_inc          ( rp_cq_order_inc   ),
    .ostatus_dbg        ( rp_cq_order_dbg   )
);

rp_cc_combine u_rp_cc_combine(
    .pcie_clk           ( pcie_clk          ),
    .pcie_rst           ( pcie_rst          ),
    .user_clk           ( user_clk          ),
    .user_rst           ( user_rst          ),
    .cc_trans_data_ex   ( cc_trans_data_ex  ),
    .cc_trans_data      ( cc_trans_data     ),
    .cc_trans_wen       ( cc_trans_wen      ),
    .cc_trans_ready     ( cc_trans_ready    ),

    .cc_oper_data_ex    ( cc_user_data_ex   ),
    .cc_oper_data       ( cc_user_data      ),
    .cc_oper_wen        ( cc_user_wen       ),
    .cc_oper_ready      ( cc_user_ready     ),

//    .read_rid           ( read_rid          ),
    .read_info          ( read_info         ),
    .read_req           ( read_req          ),
    .ostat_inc          ( rp_cc_combine_inc ),
    .ostatus_dbg        ( rp_cc_combine_dbg )
);



//stream_mux_rp u_stream_mux_rp(
//    .cbus_clk           ( cbus_clk          ),
//    .cbus_rst           ( cbus_rst          ),
//    .cfg_tx_data        ( cfg_tx_data       ),
//    .cfg_tx_wen         ( cfg_tx_wen        ),
//
//    .pcie_clk           ( pcie_clk          ),
//    .pcie_rst           ( pcie_rst          ),
//    .stream_in_data_ex  ( cq_trans_data_ex  ),
//    .stream_in_data     ( cq_trans_data     ),
//    .stream_in_wen      ( cq_trans_wen      ),
//    .stream_in_ready    ( cq_trans_ready    ),
//
//    .sys_clk            ( user_clk          ),
//    .sys_rst            ( user_rst          ),
//    .stream_out_data_ex ( chan_tx_data_ex   ),
//    .stream_out_data    ( chan_tx_data      ),
//    .stream_out_wen     ( chan_tx_wen       ),
//    .stream_out_ready   ( chan_tx_ready     ),
//
//    .ostat_inc          ( stream_mux_rp_inc ),
//    .ostatus_dbg        ( stream_mux_rp_dbg )
//);
assign chan_tx_data_ex = cq_trans_data_ex;
assign chan_tx_data    = cq_trans_data   ;
assign chan_tx_wen     = cq_trans_wen    ;
assign cq_trans_ready = chan_tx_ready;


//rmt_cfg_identify u_rmt_cfg_identify(
//    .user_clk           ( user_clk          ),
//    .user_rst           ( user_rst          ),
//
//    .mixed_data_ex      ( chan_rx_data_ex   ),
//    .mixed_data         ( chan_rx_data      ),
//    .mixed_wen          ( chan_rx_wen       ),
//    .mixed_ready        ( chan_rx_ready     ),
//
//    .cfg_rx_data        ( cfg_rx_data       ),
//    .cfg_rx_wen         ( cfg_rx_wen        ),
//
//    .pcie_clk           ( pcie_clk          ),
//    .pcie_rst           ( pcie_rst          ),
//
//    .raw_data_ex        ( raw_data_ex       ),
//    .raw_data           ( raw_data          ),
//    .raw_wen            ( raw_wen           ),
//    .raw_ready          ( raw_ready         )
//);
assign raw_data_ex = chan_rx_data_ex;
assign raw_data    = chan_rx_data   ;
assign raw_wen     = chan_rx_wen    ;
assign chan_rx_ready = raw_ready;



nvme_rp u_nvme_rp(
    .pcie_clk                   ( pcie_clk          ),
    .pcie_rst                   ( pcie_rst          ),
    .user_clk                   ( user_clk          ),
    .user_rst                   ( user_rst          ),

    .dbg_lbus_req               ( dbg_lbus_req      ),
    .dbg_lbus_type              ( dbg_lbus_type     ),
    .dbg_lbus_addr              ( dbg_lbus_addr     ),
    .dbg_lbus_wdata             ( dbg_lbus_wdata    ),
    .dbg_lbus_ack               ( dbg_lbus_ack      ),
    .dbg_lbus_rdata             ( dbg_lbus_rdata    ),

    .raw_data_ex                ( raw_data_ex       ),   //ep2rp
    .raw_data                   ( raw_data          ),
    .raw_wen                    ( raw_wen           ),
    .raw_ready                  ( raw_ready         ),

    .cq_cpl_msg_data_ex         ( cq_msg_data_ex    ),   //rp2ep
    .cq_cpl_msg_data            ( cq_msg_data       ),
    .cq_cpl_msg_wen             ( cq_msg_wen        ),
    .cq_cpl_msg_ready           ( cq_msg_ready      ),

    .cq_oper_data_ex            ( cq_local_data_ex  ),   //Pcie write/read operation
    .cq_oper_data               ( cq_local_data     ),
    .cq_oper_wen                ( cq_local_wen      ),
    .cq_oper_ready              ( cq_local_ready    ),

    .cc_oper_data_ex            ( cc_trans_data_ex  ),
    .cc_oper_data               ( cc_trans_data     ),
    .cc_oper_wen                ( cc_trans_wen      ),
    .cc_oper_ready              ( cc_trans_ready    ),

    .rq_oper_data_ex            ( rq_oper_data_ex   ),   //Pcie write operation
    .rq_oper_data               ( rq_oper_data      ),
    .rq_oper_wen                ( rq_oper_wen       ),
    .rq_oper_ready              ( rq_oper_ready     ),

    .rc_cplr_data_ex            ( rc_cplr_data_ex   ),
    .rc_cplr_data               ( rc_cplr_data      ),
    .rc_cplr_wen                ( rc_cplr_wen       ),
    .rc_cplr_ready              ( rc_cplr_ready     ),

    .cfg_mgmt_addr              ( cfg_mgmt_addr           ),
    .cfg_mgmt_write             ( cfg_mgmt_write          ),
    .cfg_mgmt_write_data        ( cfg_mgmt_write_data     ),
    .cfg_mgmt_byte_enable       ( cfg_mgmt_byte_enable    ),
    .cfg_mgmt_read              ( cfg_mgmt_read           ),
    .cfg_mgmt_read_data         ( cfg_mgmt_read_data      ),
    .cfg_mgmt_read_write_done   ( cfg_mgmt_read_write_done),

//    .curr_time                  ( curr_time_syn         ),
//    .cap_radrs                  ( cap_radrs             ),

    .pcie_lnk_up                ( pcie_lnk_up           ),
    .ds_bus_number              ( ds_bus_number         ),
    .exec_hold_ctl              ( exec_hold_ctl         ),
    .exec_hold_time             ( exec_hold_time        ),
    .set_ssd_version            ( set_ssd_version       ),
    .sq_set_tout                ( sq_set_tout           ),
    .cq_set_tout                ( cq_set_tout           ),

    .dev_onsite                 ( dev_onsite            ),
    .ssd_err                    ( ssd_err               ),
    .init_done                  ( init_done             ),
    .class_code                 ( class_code            ),
    .ssd_version                ( ssd_version           ),
    .ssd_qsize                  ( ssd_qsize             ),

    .ssd_sq_tail                ( ssd_sq_tail           ),
    .ssd_sq_head                ( ssd_sq_head           ),
    .ssd_cq_head                ( ssd_cq_head           ),
    .admin_ctl_dbg              ( admin_ctl_dbg         ),
    .io_ctl_rp_dbg              ( io_ctl_rp_dbg         ),
    .sqmsg_op_dbg               ( sqmsg_op_dbg          ),
    .sq_ctl_rp_dbg              ( sq_ctl_rp_dbg         ),
    .cq_ctl_rp_dbg              ( cq_ctl_rp_dbg         ),
    .pcie_cq_ingress_dbg        ( pcie_cq_ingress_dbg   ),
    .pcie_cc_egress_dbg         ( pcie_cc_egress_dbg    ),
    .io_ctl_rp_inc              ( io_ctl_rp_inc         ),
    .sqmsg_op_inc               ( sqmsg_op_inc          ),
    .sq_ctl_rp_inc              ( sq_ctl_rp_inc         ),
    .cq_ctl_rp_inc              ( cq_ctl_rp_inc         ),
    .pcie_cq_ingress_inc        ( pcie_cq_ingress_inc   ),
    .pcie_cc_egress_inc         ( pcie_cc_egress_inc    )
);


//rmt_cfg_intf_rp u_rmt_cfg_intf_rp(
//    .cbus_clk           ( cbus_clk      ),
//    .cbus_rst           ( cbus_rst      ),
//    .cbus_req           ( cbus_req      ),    //one pulse
//    .cbus_rw            ( cbus_rw       ),    //0:wr 1:rd
//    .cbus_addr          ( cbus_addr     ),
//    .cbus_wdata         ( cbus_wdata    ),
//    .cbus_ack           ( cbus_ack      ),
//    .cbus_0_rdata       ( cbus_0_rdata  ),
//    .cbus_1_rdata       ( cbus_1_rdata  ),
//    .cbus_2_rdata       ( cbus_2_rdata  ),
//    .cbus_3_rdata       ( cbus_3_rdata  ),
//
//    .sys_clk            ( user_clk      ),
//    .sys_rst            ( user_rst      ),
//    .cfg_tx_data        ( cfg_tx_data   ),
//    .cfg_tx_wen         ( cfg_tx_wen    ),
//    .cfg_rx_data        ( cfg_rx_data   ),
//    .cfg_rx_wen         ( cfg_rx_wen    )
//);

nvme_rp_dfx u_nvme_rp_dfx(
    .cbus_clk               ( cbus_clk              ),
    .cbus_rst               ( cbus_rst              ),
    .pcie_clk               ( pcie_clk              ),
    .user_clk               ( user_clk              ),

    .ilbus_req              ( dfx_cbus_req          ),
    .ilbus_rw               ( dfx_cbus_rw           ),   //1:rd 0:wr
    .ilbus_addr             ( dfx_cbus_addr         ),
    .ilbus_wdata            ( dfx_cbus_wdata        ),
    .olbus_ack              ( dfx_cbus_ack          ),
    .olbus_rdata            ( dfx_cbus_rdata        ),

    .soft_reset             ( ),
    .clear_stat             ( ),
    .cfg_status_dbg         ( cfg_status_dbg        ),
    .pcie_lnk_up            ( pcie_lnk_up           ),

    .pcie_manual_rst        ( pcie_manual_rst       ),
    .set_ssd_version        ( set_ssd_version       ),
    .ssd_exec_hold_ctl      ( exec_hold_ctl         ),
    .ssd_exec_hold_time     ( exec_hold_time        ),
    .cq_set_tout            ( cq_set_tout           ),
    .sq_set_tout            ( sq_set_tout           ),
    .dev_onsite             ( dev_onsite            ),
    .ssd_err                ( ssd_err               ),
    .init_done              ( init_done             ),
    .class_code             ( class_code            ),
    .ssd_version            ( ssd_version           ),
    .ssd_qsize              ( ssd_qsize             ),
    .ssd_sq_tail            ( ssd_sq_tail           ),
    .ssd_sq_head            ( ssd_sq_head           ),
    .ssd_cq_head            ( ssd_cq_head           ),
    .pcie_user_if_dbg       ( pcie_user_if_dbg      ),
    .pcie_usr_cq_dbg        ( pcie_usr_cq_dbg       ),
    .pcie_usr_cc_dbg        ( pcie_usr_cc_dbg       ),
    .admin_ctl_dbg          ( admin_ctl_dbg         ),
    .io_ctl_rp_dbg          ( io_ctl_rp_dbg         ),
    .sqmsg_op_dbg           ( sqmsg_op_dbg          ),
    .sq_ctl_rp_dbg          ( sq_ctl_rp_dbg         ),
    .cq_ctl_rp_dbg          ( cq_ctl_rp_dbg         ),
    .pcie_cq_ingress_dbg    ( pcie_cq_ingress_dbg   ),
    .pcie_cc_egress_dbg     ( pcie_cc_egress_dbg    ),
    .rp_cq_order_dbg        ( rp_cq_order_dbg       ),
    .rp_cc_combine_dbg      ( rp_cc_combine_dbg     ),
    .stream_mux_rp_dbg      ( stream_mux_rp_dbg     ),
    .io_ctl_rp_inc          ( io_ctl_rp_inc         ),
    .sqmsg_op_inc           ( sqmsg_op_inc          ),
    .sq_ctl_rp_inc          ( sq_ctl_rp_inc         ),
    .cq_ctl_rp_inc          ( cq_ctl_rp_inc         ),
    .pcie_cq_ingress_inc    ( pcie_cq_ingress_inc   ),
    .pcie_cc_egress_inc     ( pcie_cc_egress_inc    ),
    .rp_cq_order_inc        ( rp_cq_order_inc       ),
    .rp_cc_combine_inc      ( rp_cc_combine_inc     ),
    .stream_mux_rp_inc      ( stream_mux_rp_inc     )
);

endmodule