/* ************************************************************************ */ 
/* xxinvext.p - Daily Inventory Dashboard Data Extract                                */
/* COPYRIGHT JCI. INC ALL RIGHTS RESERVED. THIS IS AN UNPUBLISHED WORK.     */
/* V8:ConvertMode=Report                                                    */
/* V8:WebEnabled=No                                                         */
/* V8:RunMode=Character,Windows                                             */
/* Report to print memo purchase orders.                                    */ 
/* ************************************************************************ */
/* PROCEDURE NAME    : xxinvext.p                                           */
/* PROCEDURE TYPE    : Report                                               */
/* DESCRIPTION       :                                                      */
/* INCLUDE FILES     :                                                      */
/* CALLED BY         :                                                      */
/* CALLED PROCEDURES :                                                      */
/* ************************************************************************ */
/* CREATED BY        : Manali Pawar    #SRR0755-02     DATE: 12-August-2016 */
/* ************************************************************************ */ 

&SCOPED-DEFINE TimeStamp  STRING(MONTH(TODAY),"99") +  STRING(DAY(TODAY),"99") + STRING(YEAR(TODAY),"9999") + "-" + substring(STRING(TIME,"hh:mm:ss"),1,2) +  substring(STRING(TIME,"hh:mm:ss"),4,2) + substring(STRING(TIME,"hh:mm:ss"),7,2) 
&SCOPED-DEFINE TimeStamp1  STRING(MONTH(TODAY),"99") +  STRING(DAY(TODAY),"99") + STRING(YEAR(TODAY),"9999") 

&SCOPED-DEFINE xxinvext_1 "xxinvext.p "
&SCOPED-DEFINE xxinvext_2 "webpage-path"
&SCOPED-DEFINE xxinvext_3 "Extract Daily Inventory Data"

{mfdtitle.i "           Daily Inventory Dashboard Data Extract" }
{wbrp01.i}

define  variable  sysdate                         as date                      NO-UNDO.
define  variable  outpath                         as char                      NO-UNDO.
define  variable  v_delim                         as char                      NO-UNDO.
define  variable  m_lbl                           as char                      NO-UNDO.
define  variable  xdb                             as char                      NO-UNDO.
DEFINE VARIABLE   tot_day                         as DATE                      NO-UNDO.
define  variable  m_title                         as char                      NO-UNDO.
define  variable  full_bdname                     as char                      NO-UNDO.
define  variable  dbnm1                           as char                      NO-UNDO.
define  variable  dbnm                            as char                      NO-UNDO.
define  variable  m_outfile                       as char  label "Output file" NO-UNDO .
define  variable  tt                              as decimal                   NO-UNDO.
define  variable  v_std_cost                      as decimal                   NO-UNDO.
define  variable  tr_std_cost                     as decimal                   NO-UNDO. 
define  variable  v_rm                            as decimal                   NO-UNDO.
define  variable  v_wip                           as decimal                   NO-UNDO.
define  variable  v_fg                            as decimal                   NO-UNDO.
define  variable  v_sales                         as decimal                   NO-UNDO.
define  variable  v_qty_mtd_tot                   as decimal                   NO-UNDO.
define  variable  v_total1                        as decimal                   NO-UNDO init 0.
define  variable  v_total                         as decimal                   NO-UNDO init 0.
define  variable  v_qty_hold                      as decimal                   NO-UNDO.
define  variable  v_value_hold                    as decimal                   NO-UNDO.
define  variable  v_qty_neg                       as decimal                   NO-UNDO.
define  variable  v_value_neg                     as decimal                   NO-UNDO.
define  variable  v_qty_mtd_scrap1                as decimal                   NO-UNDO.
define  variable  v_qty_mtd_scrap2                as decimal                   NO-UNDO.
define  variable  v_qty_mtd_scrap                 as decimal                   NO-UNDO.
define  variable  v_value_mtd_scrap               as decimal                   NO-UNDO.
define  variable  v_qty_mtd_gain_sum              as decimal                   NO-UNDO.
define  variable  v_qty_mtd_loss_sum              as decimal                   NO-UNDO.
define  variable  v_value_mtd_gain                as decimal                   NO-UNDO.
define  variable  v_value_mtd_loss                as decimal                   NO-UNDO.
define  variable  v_qty_status                    as character                 NO-UNDO.
define variable   i                               as integer                   NO-UNDO.
define  variable  v_cost_mtd1                     as decimal                   NO-UNDO.
define  variable  v_cost_mtd2                     as decimal                   NO-UNDO.
define  variable  v_qty_mtd1                     as decimal                   NO-UNDO.
define  variable  v_qty_mtd2                     as decimal                   NO-UNDO.



m_title = getTermLabel("Daily Inventory Dashboard Data Extract",32).
full_bdname =  PDBNAME(1).
dbnm = entry(4,full_bdname,"/").
 m_outfile = dbnm + "-dailyinventory-" + {&timestamp} + ".csv" .   
 
{xxpath.i &outbound=yes &inbound=no &archive=no}
assign v_delim = ",".
  form
   	{&xxinvext_3} at  24 skip(1)
	m_outfile FORMAT "X(50)"
	skip(4) 
   	WITH FRAME a SIDE-LABEL  width 80.
   setFrameLabels(FRAME a:HANDLE).
display m_outfile WITH FRAME a .

DEFINE TEMP-TABLE tt_inv_data 
field tt_site_code            as character
field tt_site_desc            as character
field tt_rm                   as decimal
field tt_wip                  as decimal
field tt_fg                   as decimal
field tt_sales                as decimal
field tt_total                as decimal
field tt_qty_hold             as decimal 
field tt_value_hold           as decimal
field tt_qty_neg              as decimal 
field tt_value_neg            as decimal 
field tt_qty_ship             as decimal 
field tt_value_ship           as decimal 
field tt_prior_doh            as decimal format ">>>>9.99" 
field tt_qty_mtd_scrap        as decimal 
field tt_value_mtd_scrap      as decimal 
field tt_qty_mtd_gain_sum     as decimal
field tt_value_mtd_gain       as decimal 
field tt_qty_mtd_loss         as decimal 
field tt_qty_mtd_loss_sum     as decimal 
field tt_value_mtd_loss       as decimal
field tt_entity               like si_entity .

 FIND first code_mstr where code_fldname = {&xxinvext_1}
        and   code_value                 = {&xxinvext_2}
	and   code_cmmt                  <> " "  no-lock NO-ERROR.           
        
         IF not available code_mstr THEN
         DO:
          ASSIGN
            m_lbl = getTermLabel("OUTPUT", 6) + "_" + getTermLabel("PATH" , 5) .
	    /*{GCM is not defined for # } */
	    {pxmsg.i &msgnum = 9610 &errorlevel = 4  &msgarg1 = m_lbl}
	    pause 10 no-message.
	    return. 
          END.
          ELSE 
          ASSIGN
            outpath = code_cmmt.	 
	    file-info:filename = outpath .
	    if file-info:full-pathname = ? then do:
	    /*INVALID PATH. */
            {pxmsg.i &MSGNUM=9601 &ERRORLEVEL=4}  
            pause 10 no-message.
            return.
	  end.

	  if r-index(outpath,"/",length(outpath)) <> length(outpath) then
          assign outpath = outpath + "/" .

	    assign sysdate = today. /* system date*/

 
       Mainloop: 
       REPEAT:
        /* Output and Batch Input */
       /* PRINTER SELECTION */

        {
     gpselout.i
       &printType                = "printer"
       &printWidth               = 132
       &pagedFlag                = " "
       &stream                   = " "
       &appendToFile             = " "
       &streamedOutputToTerminal = " "
       &withBatchOption          = "yes"
       &displayStatementType     = 1
       &withCancelMessage        = "yes"
       &pageBottomMargin         = 6
       &withEmail                = "yes"
       &withWinprint             = "yes"
       &defineVariables          = "yes"
   }

        assign tot_day = date(  MONTH(TODAY) , 1  , YEAR(TODAY)).
       /*for v_std_cost*/
       for each ld_det no-lock ,
               first si_mstr  where si_site  = ld_site  no-lock,
	       first pt_mstr  where pt_part  = ld_part  no-lock,
	       first loc_mstr where loc_loc =  ld_loc and loc_site = ld_site  break by ld_site by ld_part 
	       :
	          if first-of(ld_site) then do:
	             CREATE  tt_inv_data.
	             /*3*/
	             assign tt_site_desc = si_desc
		     tt_site_code = ld_site
	              v_rm                 = 0
	              v_wip                = 0
	              v_fg                 = 0
	              v_sales              = 0
		      v_qty_mtd_tot        = 0  
                      v_total              = 0   
                      v_qty_hold           = 0
                      v_value_hold         = 0  
                      v_qty_neg            = 0
                      v_value_neg          = 0
                      v_qty_mtd_scrap      = 0 
                      v_value_mtd_scrap    = 0 
                      v_qty_mtd_gain_sum   = 0 
                      v_qty_mtd_loss_sum   = 0
                      v_value_mtd_gain     = 0
                      v_value_mtd_loss     = 0 .
	           end.  /*first-of(ld-site)*/

	         if first-of(ld_part) then do:
	              for first sct_det where sct_site = ld_site and sct_part = ld_part:
                          assign v_std_cost  = sct_cst_tot.
 	              end. /*sct_det*/
	              if not available sct_det then  v_std_cost = 0. 
                 end. /*first-of(ld-part)*/

	        /*4*/
	        if  loc_type = "RM" and 
	               not (can-find( first code_mstr where code_fldname  = "xxinvext.p_obs" and
                                                            code_value    = pt_status no-lock)) then do:
		                  v_rm = v_rm + ld_qty_oh *  v_std_cost. end.
	        /*5*/
	        if loc_type = "WIP" and 
	               not (can-find( first code_mstr where code_fldname  = "xxinvext.p_obs" and
                                                            code_value    = pt_status no-lock)) then 
		                  v_wip = v_wip + ld_qty_oh *  v_std_cost.
	       /*6*/
	       if loc_type = "FG" and 
	               not (can-find( first code_mstr where code_fldname  = "xxinvext.p_obs" and
                                                            code_value    = pt_status no-lock)) then 
		                  v_fg = v_fg + ld_qty_oh *  v_std_cost.
	       /*7*/
	       if loc_type = "SALES" then  
		      v_sales = v_sales + ld_qty_oh *  v_std_cost.

	       /*8*/
               v_total = v_total + ( ld_qty_oh *  v_std_cost ).

               /*9 , 10*/
               if loc_type = "OH" then  do:
		     v_qty_hold = v_qty_hold + ld_qty_oh.
                     v_value_hold = v_value_hold + ( ld_qty_oh * v_std_cost ).
               end.
	      /*11*/ 
	      if ld_qty_oh < 0 then 
                     v_qty_neg = v_qty_neg + ld_qty_oh .
	     /*12*/
	     v_value_neg = v_value_neg + ( ld_qty_oh * v_std_cost ).
	    if last-of(ld_site) then do:
	       assign 
	          v_qty_mtd1  = 0
	          v_qty_mtd2  = 0
		  v_cost_mtd1 = 0
		  v_cost_mtd2 = 0.

                /*16 , 17*/
                for each tr_hist where tr_type = "ISS-SCRP" and tr_date >= tot_day and tr_date <= today and tr_site = ld_site:
	              assign tr_std_cost = tr_bdn_std + tr_ovh_std + tr_mtl_std + tr_lbr_std + tr_sub_std.
        	      v_qty_mtd1 = v_qty_mtd1 + tr_qty_loc.
		      v_cost_mtd1 = v_cost_mtd1 + tr_qty_loc * tr_std_cost.
	        end. /*tr_hist*/

               for each tr_hist where tr_type = "ISS-UNP" and tr_date >= tot_day and tr_date <= today and tr_site = ld_site no-lock,
	                                                                        first trgl_det where  trgl_trnbr = tr_trnbr
	                                                       and can-find( first code_mstr where code_fldname  = "xxinvext.p_scrapacct" and
							                                           code_value    = trgl_dr_acct no-lock) :
                      assign tr_std_cost = tr_bdn_std + tr_ovh_std + tr_mtl_std + tr_lbr_std + tr_sub_std.                                                                              
                      v_qty_mtd2 = v_qty_mtd2 + tr_qty_loc.
		      v_cost_mtd2 = v_cost_mtd2 + tr_qty_loc * tr_std_cost.
	       end. /*tr_hist*/
	           v_qty_mtd_scrap = v_qty_mtd1 + v_qty_mtd2.
                   v_value_mtd_scrap = v_cost_mtd1 + v_cost_mtd2.
            
   	      /*18,19,20, 21*/
                 for each tr_hist where tr_type begins "cyc" or  tr_type begins "tag"
                                          and tr_date >= tot_day and tr_date <= today 
		 	                   and ld_site = tr_site  break by tr_part :
	               if tr_type = "cyc-err" then next.
	            if first-of(tr_part) then do:
		       v_qty_mtd1 = 0.
	               v_cost_mtd1 = 0.
	           end. /*first-of(tr_part)*/
                 assign tr_std_cost = tr_bdn_std + tr_ovh_std + tr_mtl_std + tr_lbr_std + tr_sub_std.
	         v_qty_mtd1 = v_qty_mtd1 + tr_qty_loc.
                 v_cost_mtd1 = v_cost_mtd1 + tr_qty_loc * tr_std_cost.

	         if last-of(tr_part) then do:
	             if v_qty_mtd_tot > 0 then  do:
			   v_qty_mtd_gain_sum = v_qty_mtd_gain_sum + v_qty_mtd_tot.
			   v_value_mtd_gain = v_value_mtd_gain + v_cost_mtd1 .
		     end. /*v_qty_mtd_tot > 0*/
	             if v_qty_mtd_tot < 0 then do:
		          v_qty_mtd_loss_sum = v_qty_mtd_loss_sum + v_qty_mtd_tot.
		            v_value_mtd_loss = v_value_mtd_loss + v_cost_mtd1.
      		     end. /*v_qty_mtd_tot < 0 */
                  end. /*last-of(tr_part)*/
            end. /*tr_hist where tr_type*/

	  assign
         tt_rm                  =  v_rm         
         tt_wip                 =  v_wip        
         tt_fg                  =  v_fg        
         tt_sales               =  v_sales        
         tt_total               =  v_total - v_sales      
         tt_qty_hold            =  v_qty_hold       
         tt_value_hold          =  v_value_hold      
         tt_qty_neg             =  v_qty_neg       
         tt_value_neg           =  v_value_neg     
  /*       tt_qty_ship            =  v_qty_ship       
         tt_value_ship          =  v_value_ship      
         tt_prior_doh           =  v_prior_doh        */ 
         tt_qty_mtd_scrap       =  v_qty_mtd_scrap     
         tt_value_mtd_scrap     =  v_value_mtd_scrap     
         tt_qty_mtd_gain_sum    =  v_qty_mtd_gain_sum     
         tt_value_mtd_gain      =  v_value_mtd_gain  
         tt_qty_mtd_loss_sum    =  v_qty_mtd_loss_sum       
         tt_value_mtd_loss      =  v_value_mtd_loss .

         end. /*last-of(ld_site)*/
  end. /*ld_det*/

    OUTPUT TO value(m_workpath + m_outfile) convert source session:cpstream target "UTF-8".
        put unformatted 
                "Date"                                  v_delim
		"Site Code"                             v_delim
		"Site Description"		       	v_delim
		"RM - Value On Hand"	 		v_delim
		"WIP - Value On Hand"			v_delim
		"FG -Value On Hand"			v_delim
		"Sales- Value On Hand"			v_delim
		"Total Value On Hand"			v_delim
		"QTY-Quality Hold"			v_delim
		"Value-Quality Hold"			v_delim
		"QTY-Total Negative"			v_delim
		"Value-Total Negative"			v_delim
		"QTY-Prior Day's shipment"		v_delim
		"Value-Prior Day's shipment(COGS)"	v_delim
	        "Prior Day's DOH"			v_delim
		"QTY-MTD Scrap"				v_delim
		"Value-MTD Scrap"			v_delim
		"QTY- MTD Cycle Count Gain"		v_delim
		"Value-MTD Cycle Count Gain"		v_delim
		"QTY- MTD Cycle Count Loss"		v_delim
		"Value- MTD Cycle Count Loss"		v_delim
		SKIP.

       for each tt_inv_data break by tt_entity:
        if first-of(tt_entity) then 
           assign i = 0.
           i = i + 1.
	   accumulate tt_rm (total by tt_entity).
	   accumulate tt_wip (total by tt_entity).
	   accumulate tt_fg (total by tt_entity).
	   accumulate tt_sales (total by tt_entity).
           accumulate tt_total (total by tt_entity).
           accumulate tt_qty_hold (total by tt_entity).
           accumulate tt_value_hold (total by tt_entity).
           accumulate tt_qty_neg (total by tt_entity).
           accumulate tt_value_neg (total by tt_entity).
           accumulate tt_qty_ship (total by tt_entity).
           accumulate tt_value_ship (total by tt_entity).
           accumulate tt_prior_doh (total by tt_entity).
           accumulate tt_qty_mtd_scrap (total by tt_entity).
           accumulate tt_value_mtd_scrap (total by tt_entity).
           accumulate tt_qty_mtd_gain_sum (total by tt_entity).
           accumulate tt_value_mtd_gain (total by tt_entity).
           accumulate tt_qty_mtd_loss_sum (total by tt_entity).
           accumulate tt_value_mtd_gain (total by tt_entity).
           accumulate tt_qty_mtd_loss_sum (total by tt_entity).
           accumulate tt_value_mtd_loss (total by tt_entity).
	   

       
	   put unformatted 
                sysdate            v_delim
                tt_site_code       v_delim 
                tt_site_desc       v_delim       
                tt_rm              v_delim         
                tt_wip             v_delim        
                tt_fg              v_delim        
                tt_sales           v_delim        
                tt_total           v_delim      
                tt_qty_hold        v_delim       
                tt_value_hold      v_delim      
                tt_qty_neg         v_delim       
                tt_value_neg       v_delim     
          /*    tt_qty_ship        v_delim       
                tt_value_ship      v_delim       
                tt_prior_doh       v_delim        */
                tt_qty_mtd_scrap   v_delim     
                tt_value_mtd_scrap     v_delim     
                tt_qty_mtd_gain_sum    v_delim     
                tt_value_mtd_gain  v_delim  
                tt_qty_mtd_loss_sum    v_delim       
                tt_value_mtd_loss  v_delim    
             skip.

	if last-of(tt_entity) and i > 1 then do:
	  for first en_mstr  where en_entity = tt_entity  :
	   put unformatted 
	   sysdate            
           tt_entity       
           en_name       
	   accum  total tt_rm               v_delim
	   accum  total tt_wip              v_delim
	   accum  total tt_fg               v_delim
	   accum  total tt_sales            v_delim
           accum  total tt_total            v_delim
           accum  total tt_qty_hold         v_delim
           accum  total tt_value_hold       v_delim
           accum  total tt_qty_neg          v_delim
           accum  total tt_value_neg        v_delim
           accum  total tt_qty_ship         v_delim
           accum  total tt_value_ship       v_delim
           accum  total tt_prior_doh        v_delim
           accum  total tt_qty_mtd_scrap    v_delim
           accum  total tt_value_mtd_scrap  v_delim
           accum  total tt_qty_mtd_gain_sum v_delim
           accum  total tt_value_mtd_gain   v_delim
           accum  total tt_qty_mtd_loss_sum v_delim
           accum  total tt_value_mtd_gain   v_delim
           accum  total tt_qty_mtd_loss_sum v_delim
           accum  total tt_value_mtd_loss   v_delim
	   skip. 
	   end. /*for first en_mstr*/
          end. /*if last-of(tt_entity)*/
       end. /*for each tt_inv_data*/
 
   output close.  /* Close the output */ 

   {mfrpexit.i}  
END.  /* END REPEAT */
  { wbrp04.i &frame-spec = a } /* DT2 Desktop */
	 
