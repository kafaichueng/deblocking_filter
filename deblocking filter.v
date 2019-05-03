//边界强度选择
module BScalculate(DCT_para_flag, intra_code_flag, macro_edge_flag,
				   same_frame_flag, same_vector_flag, BS, clk);

input  DCT_para_flag, intra_code_flag, macro_edge_flag, same_frame_flag, same_vector_flag;
output reg[2:0]BS;

always@(DCT_para_flag or intra_code_flag or macro_edge_flag or same_frame_flag or same_vector_flag)
	
	if(intra_code_flag == 1'd1)
		if(macro_edge_flag == 1'd1)
			BS <= 3'd4;
		else
			BS <= 3'd3;
	else
		if(DCT_para_flag == 1'd0)
			if(same_frame_flag == 1)
				BS <= 3'd1;
			else
				if(same_vector_flag == 1)
					BS <= 3'd1;
				else 
					BS <= 3'd0;
		else
			BS <= 3'd2;

endmodule
//*********************************************************************
//**********************************************************************

//寄存器1
module flip_flop1(BS, indexA, indexB, p0, p1, p2, p3, q0, q1, q2, q3, clk,
				ff1BS, ff1indexA, ff1indexB, ff1p0, ff1p1, ff1p2, ff1p3, ff1q0, ff1q1, ff1q2, ff1q3);
				
input BS, indexA, indexB, p0, p1, p2, p3, q0, q1, q2, q3, clk;
output reg[2:0]ff1BS;
output reg[5:0]ff1indexA, ff1indexB;
output reg[7:0]ff1p0, ff1p1, ff1p2, ff1p3, ff1q0, ff1q1, ff1q2, ff1q3;

always@(posedge clk)begin
if(clk)begin
	ff1BS <= BS;
	ff1indexA <= indexA;
	ff1indexB <= indexB;
	ff1q0 <= q0;
	ff1q1 <= q1;
	ff1q2 <= q2;
	ff1q3 <= q3;
	ff1p0 <= p0;
	ff1p1 <= p1;
	ff1p2 <= p2;
	ff1p3 <= p3;
	end
end
endmodule
endmodule
//************************************************************************

//寄存器2
module flip_flop2(BSout, indexAout, indexBout, Tp0, Tp1, Tp2, Tp3, Tq0, Tq1, Tq2, Tq3, filter_flag, beta, alpha, clk,
				ff2BS, ff2indexA, ff2indexB, ff2p0, ff2p1, ff2p2, ff2p3, ff2q0, ff2q1, ff2q2, ff2q3, ff2alpha, ff2beta, ff_filter_flag);
				
input BSout, indexAout, indexBout, Tp0, Tp1, Tp2, Tp3, Tq0, Tq1, Tq2, Tq3, beta, alpha, filter_flag, clk;
output reg[2:0]ff2BS;
output reg[5:0]ff2indexA, ff2indexB;
output reg[7:0]ff2p0, ff2p1, ff2p2, ff2p3, ff2q0, ff2q1, ff2q2, ff2q3;
output reg[7:0]ff2alpha;
output reg[4:0]ff2beta;
output reg ff_filter_flag;

always@(posedge clk)begin
	if(clk)begin
	ff2BS <= BSout;
	ff2indexA <= indexAout;
	ff2indexB <= indexBout;
	ff2p0 <= Tp0;
	ff2p1 <= Tp1;
	ff2p2 <= Tp2;
	ff2p3 <= Tp3;
	ff2q0 <= Tq0;
	ff2q1 <= Tq1;
	ff2q2 <= Tq2;
	ff2q3 <= Tq3;
	ff2alpha <= alpha;
	ff2beta <= beta;
	ff_filter_flag <= filter_flag;
	end
end
endmodule
//*********************************************************************

//alpha查表部分
module ALPHAtable(alpha, indexA);
input  [5:0]indexA;
output reg[7:0]alpha;

always@(indexA)begin
	case(indexA)
		6'd16, 6'd17:alpha <=8'd4;
		6'd18:alpha <= 8'd5;
		6'd19:alpha <= 8'd6;
		6'd20:alpha <= 8'd6;
		6'd21:alpha <= 8'd8;
		6'd22:alpha <= 8'd9;
		6'd23:alpha <= 8'd8;
		6'd24:alpha <= 8'd12;
		6'd25:alpha <= 8'd13;
		6'd26:alpha <= 8'd15;
		6'd27:alpha <= 8'd17;
		6'd28:alpha <= 8'd20;
		6'd29:alpha <= 8'd22;
		6'd30:alpha <= 8'd25;
		6'd31:alpha <= 8'd28;
		6'd32:alpha <= 8'd32;
		6'd33:alpha <= 8'd36;
		6'd34:alpha <= 8'd40;//40
		6'd35:alpha <= 8'd46;//46
		6'd36:alpha <= 8'd50;
		6'd37:alpha <= 8'd56;
		6'd38:alpha <= 8'd63;
		6'd39:alpha <= 8'd71;
		6'd40:alpha <= 8'd80;
		6'd41:alpha <= 8'd90;
		6'd42:alpha <= 8'd81;
		6'd43:alpha <= 8'd113;
		6'd44:alpha <= 8'd127;
		6'd45:alpha <= 8'd144;
		6'd46:alpha <= 8'd162;
		6'd47:alpha <= 8'd182;
		6'd48:alpha <= 8'd203;
		6'd49:alpha <= 8'd226;
		6'd50:alpha <= 8'd255;
		6'd51:alpha <= 8'd255;
	default:alpha <= 8'd0;
	endcase
end
endmodule
//*******************************************************************

//beta查表代码
module BETAtable(beta, indexB );
input  [5:0]indexB;
output reg[4:0]beta;

always@(indexB)begin
	case(indexB)
		6'd16, 6'd17, 6'd18:beta <= 5'd2;
		6'd19, 6'd20, 6'd21, 6'd22:beta <= 5'd3;
		6'd23, 6'd24, 6'd25:beta <= 5'd5;
		6'd26, 6'd27:beta <= 5'd6;
		6'd28, 6'd29:beta <= 5'd7;
		6'd30, 6'd31:beta <= 5'd8;
		6'd32, 6'd33:beta <= 5'd9;
		6'd34, 6'd35:beta <= 5'd10;
		6'd36, 6'd37:beta <= 5'd11;
		6'd38, 6'd39:beta <= 5'd12;
		6'd40, 6'd41:beta <= 5'd13;
		6'd42, 6'd43:beta <= 5'd14;
		6'd44, 6'd45:beta <= 5'd15;
		6'd46, 6'd47:beta <= 5'd16;
		6'd48, 6'd49:beta <= 5'd17;
		6'd50, 6'd51:beta <= 5'd18;
	default:beta <= 5'd0;
	endcase
end
endmodule
//********************************************************************
//indexA和indexB计算
module Th_calculate1(indexA, indexB, QP_q, QP_p, offsetA, offsetB);

input  QP_p, QP_q, offsetA, offsetB;
output reg[5:0]indexA;
output reg[5:0]indexB;

wire   [4:0]QP_v;

assign QP_v = (QP_p + QP_q + 1) >> 1;

always@(QP_v or offsetA or offsetB)

begin
	if(QP_v + offsetA <= 1'd0)
		indexA <= 6'd0;
	else if(QP_v + offsetA <= 7'd51)
		indexA <= 6'd51;
	else
		indexA <= QP_v + offsetA;
		
	if(QP_v + offsetB <= 0)
		indexB <= 6'd0;
	else if(QP_v + offsetB <= 7'd51)
		indexB <= 6'd51;
	else
		indexB <= QP_v + offsetB;		
end

endmodule
//***********************************************************
//预计算2
module Th_calculate2(p0, p1, p2, p3, q0, q1, q2, q3, indexA, indexB, alpha, beta, BS, filter_flag, BSout, 
					indexAout, indexBout, Tp0, Tp1, Tp2, Tp3, Tq0, Tq1, Tq2, Tq3);

input  p0, p1, p2, p3, q0, q1, q2, q3,indexA, indexB, BS;

output reg[7:0]alpha, Tp0, Tp1, Tp2, Tp3, Tq0, Tq1, Tq2, Tq3;
output reg[4:0]beta;
output reg filter_flag;
output reg[5:0]indexAout, indexBout;
output reg[2:0]BSout;

wire   [4:0]w2;
wire   [7:0]w1;
wire   [7:0]abs1, abs2, abs3;

ALPHAtable E1(w1, indexA);
BETAtable  E2(w2, indexB);
abs_calculate E3(p0, q0, abs1);
abs_calculate E4(p1, p0, abs2);
abs_calculate E5(q1, q0, abs3);

always@(w1 or w2 or abs1 or abs2 or abs3 or BS or indexA or indexB or p0 or p1 or p2 or p3 or q0 or q1 or q2 or q3)begin
	alpha <= w1;
	beta  <= w2;
	BSout <= BS;
	indexAout <= indexA;
	indexBout <= indexB;
	Tq0 <= q0;
	Tq1 <= q1;
	Tq2 <= q2;
	Tq3 <= q3;
	Tp0 <= p0;
	Tp1 <= p1;
	Tp2 <= p2;
	Tp3 <= p3;
	filter_flag <= (abs1 < w1 && abs2 < w2 && abs3 < w2)?(1'd1):(1'd0);
end
endmodule
//*******************************************************************

//************************************************************

//c0查表部分
module c0table(BS, indexA, c0);

input  BS;
inout  [5:0]indexA;
output reg[5:0]c0;

always@(BS or indexA)begin
	if(BS == 1'd1)
			case(indexA)
				6'd23,6'd24,6'd25,6'd26,6'd27,6'd28,6'd29,6'd30,6'd31,6'd32:c0 <= 6'd1;
				6'd33,6'd34,6'd35,6'd36:c0 <= 6'd2;
				6'd37,6'd38,6'd39:c0 <= 6'd3;
				6'd40,6'd41,6'd42:c0 <= 6'd4;
				6'd43:c0 <= 6'd5;
				6'd44,6'd45:c0 <= 6'd6;
				6'd46:c0 <= 6'd7;
				6'd47:c0 <= 6'd8;
				6'd48:c0 <= 6'd9;
				6'd49:c0 <= 6'd10;
				6'd50:c0 <= 6'd11;
				6'd51:c0 <= 6'd13;
				default:c0 <= 6'd0;
			endcase
	else if(BS == 2'd2)
			case(indexA)
				6'd21,6'd22,6'd23,6'd24,6'd25,6'd26,6'd27,6'd28,6'd29,6'd30:c0 <= 6'd1;
				6'd31,6'd32,6'd33,6'd34:c0 <= 6'd1;
				6'd35,6'd36,6'd37:c0 <= 6'd3;
				6'd38,6'd39:c0 <= 6'd4;
				6'd40,6'd41:c0 <= 6'd5;
				6'd42:c0 <= 6'd6;
				6'd43:c0 <= 6'd7;
				6'd44,6'd45:c0 <= 6'd8;
				6'd46:c0 <= 6'd10;
				6'd47:c0 <= 6'd11;
				6'd48:c0 <= 6'd12;
				6'd49:c0 <= 6'd13;
				6'd50:c0 <= 6'd15;//
				6'd51:c0 <= 6'd17;
				default:c0 <= 6'd0;
			endcase
	else if(BS == 2'd3)
			case(indexA)
				6'd17,6'd18,6'd19,6'd20,6'd21,6'd22,6'd23,6'd24,6'd25,6'd26:c0 <= 6'd1;
				6'd27,6'd28,6'd29,6'd30:c0 <= 6'd2;
				6'd31,6'd32,6'd33:c0 <= 6'd3;
				6'd34,6'd35,6'd36:c0 <= 6'd4;
				6'd37:c0 <= 6'd5;
				6'd38,6'd39:c0 <= 6'd6;
				6'd40:c0 <= 6'd7;
				6'd41:c0 <= 6'd8;
				6'd42:c0 <= 6'd9;
				6'd43:c0 <= 6'd10;
				6'd44:c0 <= 6'd11;
				6'd45:c0 <= 6'd13;
				6'd46:c0 <= 6'd15;
				6'd47:c0 <= 6'd16;
				6'd48:c0 <= 6'd18;
				6'd49:c0 <= 6'd20;
				6'd50:c0 <= 6'd23;
				6'd51:c0 <= 6'd25;
				default:c0 <=6'd0;
			endcase
		else c0 <= 6'd0;
end
endmodule

//***************************************************************
//滤波计算部分
module filtercalculate(p0, p1, p2, p3, q0, q1, q2, q3, fp0, fp1, fp2, fp3,
						fq0, fq1, fq2, fq3, BSout, indexAout, indexBout, alpha, beta, filter_flag);
				
input  p0, p1, p2, p3, q0, q1, q2, q3, BSout, indexAout, indexBout, alpha, beta, filter_flag;
output reg[7:0]fp0, fp1, fp2, fp3;
output reg[7:0]fq0, fq1, fq2, fq3;

wire   [7:0]abs1, abs2;
wire   [7:0]delta_q0, delta_q0i, delta_q1, delta_q1i, delta_p1, delta_p1i;
wire   [5:0]c0;
wire   [1:0]str_flag;

c0table Z3(BSout, indexAout, c0);
abs_calculate Z1(p0, q0, abs1);
abs_calculate Z2(p2, p0, abs2);

assign delta_q0i = ((3'd4*(q0 - p0) + p1 - q1 + 3'd4) >> 1'd1);
assign delta_q1i = ((p2 + ((q0 + p0 + 1'd1) >> 1'd1) - 2'd2*p1) >> 1'd1);
assign delta_p1i = ((q2 + ((q0 + p0 + 1'd1) >> 1'd1) - 2'd2*q1) >> 1'd1);
assign delta_q0 = ((delta_q0i <= -c0)?(-c0):((delta_q0i >= c0)?(c0):(delta_q0i)));
assign delta_p1 = ((delta_p1i <= -c0 - 1'd1)?(-c0 - 1'd1):((delta_p1i >= c0 + 1'd1)?(c0 + 1'd1):(delta_p1i)));
assign delta_q1 = ((delta_q1i <= -c0 - 1'd1)?(-c0 - 1'd1):((delta_q1i <= c0 + 1'd1)?(c0 + 1'd1):(delta_q1i)));
assign str_flag = ((abs1 < ((alpha >> 2'd2) + 2'd2)) && (abs2 < beta))?(1'd1):(1'd0);

always@(p0 or p1 or p2 or p3 or q0 or q1 or q2 or q3 or str_flag or filter_flag
		or delta_q0 or delta_p1 or delta_q1 or BSout or alpha or beta)begin

 if(1'd1 <=BSout<= 2'd3)begin
	fp0 <= p0 + delta_q0;
	fp1 <= p1 + delta_p1;
	fp2 <= p2;
	fp3 <= p3;
    fq0 <= q0 + delta_q0;
	fq1 <= q1 + delta_q1;
	fq2 <= q2;
	fq3 <= q3;
	end
 else if(BSout == 3'd4 && str_flag == 1'd0)begin
	fp0 <= (2'd2*p1 + p0 + q1 + 2'd2) >> 2'd2;
	fp1 <= p1;
	fp2 <= p2;
	fp3 <= p3;
	fq0 <= (2'd2*q1 + q0 + p1 + 2'd2) >> 2'd2;
	fq1 <= q1;
	fq2 <= q2;
	fq3 <= q3;
	end
 else if(BSout == 3'd4 && str_flag == 1'd1)begin
	fp0 <= (p2 + 2'd2*p1 + 2'd2*p0 + 2'd2*q0 + q1 + 3'd4) >> 2'd3;
	fp1 <= (p2 + p1 + p0 + q0 + 1'd2) >> 2'd2;
	fp2 <= (2'd2*p3 + 2'd3*p2 + p1 + p0 + q0 + 3'd4) >> 2'd3;
	fp3 <= p3;
	fq0 <= ((p1 + 2'd2*p0 + 2'd2*q0 + q2 + 3'd4) >> 2'd3);
	fq1 <= (p0 + q0 + q1 + q2 + 2'd2) >> 2'd2;
	fq2 <= (2'd2*q3 + 2'd3*q2 + q1 + q0 + p0 + 3'd4) >> 2'd3;
	fq3 <= q3;
	end

 else begin
	fp0 <= p0;
	fp1 <= p1;
	fp2 <= p2;
	fp3 <= p3;
	fp0 <= p0;
	fp1 <= p1;
	fp2 <= p2;
	fp3 <= p3;
 end
end
endmodule
//*****************************************************************

//顶层模块
module TOPLEVEL(DCT_para_flag, intra_code_flag, macro_edge_flag, same_frame_flag, same_vector_flag,
				p0, p1, p2, p3, q0, q1, q2, q3, out_p0, out_p1, out_p2, out_p3,
				out_q0, out_q1, out_q2, out_q3, QP_q, QP_p, offsetA, offsetB, clk);
				
input  DCT_para_flag, intra_code_flag, macro_edge_flag, same_frame_flag, same_vector_flag, clk;
input  [7:0]p0, p1, p2, p3, q0, q1, q2, q3;
input  [4:0]QP_q, QP_p, offsetA, offsetB;

output reg[7:0]out_p0, out_p1, out_p2, out_p3, out_q0, out_q1, out_q2, out_q3;

wire   [5:0]indexA, indexB, indexAout, indexBout, ff1indexA, ff1indexB, ff2indexA, ff2indexB;
wire   [7:0]alpha, ff2alpha;
wire   [4:0]beta, ff2beta; 
wire   [7:0]op0, op1, op2, op3, oq0, oq1, oq2, oq3;
wire   [2:0]BS, BSout, ff1BS, ff2BS;
wire   filter_flag, ff_filter_flag;
wire   [7:0]Tp0, Tp1, Tp2, Tp3, Tq0, Tq1, Tq2, Tq3, ff1p0, ff1p1, ff1p2, ff1p3, ff1q0, ff1q1;
wire   [7:0]ff1q2, ff1q3, ff2p0, ff2p1, ff2p2, ff2p3, ff2q0, ff2q1, ff2q2, ff2q3;

	BScalculate M1(DCT_para_flag, intra_code_flag, macro_edge_flag, same_frame_flag, same_vector_flag, BS);
	
	Th_calculate1 M2(indexA, indexB, QP_q, QP_p, offsetA, offsetB);
		
	flip_flop1 M3(BS, indexA, indexB, p0, p1, p2, p3, q0, q1, q2, q3, clk, 
				ff1BS, ff1indexA, ff1indexB, ff1p0, ff1p1, ff1p2, ff1p3, ff1q0, ff1q1, ff1q2, ff1q3);
					
    Th_calculate2 M4(ff1p0, ff1p1, ff1p2, ff1p3, ff1q0, ff1q1, ff1q2, ff1q3, indexA, indexB, alpha, beta, 
					ff1BS, filter_flag, BSout, indexAout, indexBout, Tp0, Tp1, Tp2, Tp3, Tq0, Tq1, Tq2, Tq3);
	
	flip_flop2 M5(BSout, indexAout, indexBout, Tp0, Tp1, Tp2, Tp3, Tq0, Tq1, Tq2, Tq3, filter_flag, beta, alpha, clk,
				  ff2BS, ff2indexA, ff2indexB, ff2p0, ff2p1, ff2p2, ff2p3, ff2q0, ff2q1, ff2q2, ff2q3, ff2alpha, ff2beta, ff_filter_flag);
				
	filtercalculate M6(ff2p0, ff2p1, ff2p2, ff2p3, ff2q0, ff2q1, ff2q2, ff2q3, op0, op1, op2, op3, oq0, 
					  oq1, oq2, oq3, ff2BS, ff2indexA, ff2indexB, ff2alpha, ff2beta, ff_filter_flag);
						
always@(op0 or op1 or op2 or op3 or oq0 or oq1 or oq2 or oq3)begin
	out_p0 <= op0;
	out_p1 <= op1;
	out_p2 <= op2;
	out_p3 <= op3;
	out_q0 <= oq0;
	out_q1 <= oq1;
	out_q2 <= oq2;
	out_q3 <= oq3;
end
endmodule
//***********************************************************************
//绝对值计算
module abs_calculate(a, b, abs);

input  a, b;
output reg[7:0]abs;

always@(a or b)begin

if(a >= b)
	abs <= a - b;
else
	abs <= b - a;
	
end
endmodule

//*******************************************************************************













//*********************************************************************************
//边界强度选择
module BScalculate(DCT_para_flag, intra_code_flag, macro_edge_flag,
				   same_frame_flag, same_vector_flag, BS, clk, enable, reset);

input  DCT_para_flag, intra_code_flag, macro_edge_flag, same_frame_flag, same_vector_flag;
output reg[2:0]BS;

always@(DCT_para_flag or intra_code_flag or macro_edge_flag or same_frame_flag or same_vector_flag)
	
	if(intra_code_flag == 1'd1)
		if(macro_edge_flag == 1'd1)
			BS <= 3'd4;
		else
			BS <= 3'd3;
	else
		if(DCT_para_flag == 1'd0)
			if(same_frame_flag == 1)
				BS <= 3'd1;
			else
				if(same_vector_flag == 1)
					BS <= 3'd1;
				else 
					BS <= 3'd0;
		else
			BS <= 3'd2;

endmodule
//*********************************************************************
//**********************************************************************
//*********************************************************************

//alpha查表部分
module ALPHAtable(alpha, indexA);
input  [5:0]indexA;
output reg[7:0]alpha;

always@(indexA)begin
	case(indexA)
		6'd16, 6'd17:alpha <=8'd4;
		6'd18:alpha <= 8'd5;
		6'd19:alpha <= 8'd6;
		6'd20:alpha <= 8'd6;
		6'd21:alpha <= 8'd8;
		6'd22:alpha <= 8'd9;
		6'd23:alpha <= 8'd8;
		6'd24:alpha <= 8'd12;
		6'd25:alpha <= 8'd13;
		6'd26:alpha <= 8'd15;
		6'd27:alpha <= 8'd17;
		6'd28:alpha <= 8'd20;
		6'd29:alpha <= 8'd22;
		6'd30:alpha <= 8'd25;
		6'd31:alpha <= 8'd28;
		6'd32:alpha <= 8'd32;
		6'd33:alpha <= 8'd36;
		6'd34:alpha <= 8'd40;//40
		6'd35:alpha <= 8'd46;//46
		6'd36:alpha <= 8'd50;
		6'd37:alpha <= 8'd56;
		6'd38:alpha <= 8'd63;
		6'd39:alpha <= 8'd71;
		6'd40:alpha <= 8'd80;
		6'd41:alpha <= 8'd90;
		6'd42:alpha <= 8'd81;
		6'd43:alpha <= 8'd113;
		6'd44:alpha <= 8'd127;
		6'd45:alpha <= 8'd144;
		6'd46:alpha <= 8'd162;
		6'd47:alpha <= 8'd182;
		6'd48:alpha <= 8'd203;
		6'd49:alpha <= 8'd226;
		6'd50:alpha <= 8'd255;
		6'd51:alpha <= 8'd255;
	default:alpha <= 8'd0;
	endcase
end
endmodule
//*******************************************************************

//beta查表代码
module BETAtable(beta, indexB );
input  [5:0]indexB;
output reg[4:0]beta;

always@(indexB)begin
	case(indexB)
		6'd16, 6'd17, 6'd18:beta <= 5'd2;
		6'd19, 6'd20, 6'd21, 6'd22:beta <= 5'd3;
		6'd23, 6'd24, 6'd25:beta <= 5'd5;
		6'd26, 6'd27:beta <= 5'd6;
		6'd28, 6'd29:beta <= 5'd7;
		6'd30, 6'd31:beta <= 5'd8;
		6'd32, 6'd33:beta <= 5'd9;
		6'd34, 6'd35:beta <= 5'd10;
		6'd36, 6'd37:beta <= 5'd11;
		6'd38, 6'd39:beta <= 5'd12;
		6'd40, 6'd41:beta <= 5'd13;
		6'd42, 6'd43:beta <= 5'd14;
		6'd44, 6'd45:beta <= 5'd15;
		6'd46, 6'd47:beta <= 5'd16;
		6'd48, 6'd49:beta <= 5'd17;
		6'd50, 6'd51:beta <= 5'd18;
	default:beta <= 5'd0;
	endcase
end
endmodule
//********************************************************************
//indexA和indexB计算
module Th_calculate1(indexA, indexB, QP_q, QP_p, offsetA, offsetB);

input  QP_p, QP_q, offsetA, offsetB;
output reg[5:0]indexA;
output reg[5:0]indexB;

wire   [7:0]QP_v;

assign QP_v = (QP_p + QP_q + 1) >> 1;

always@(QP_v or offsetA or offsetB)

begin
	if(QP_v + offsetA <= 1'd0)
		indexA <= 6'd0;
	else if(QP_v + offsetA <= 7'd51)
		indexA <= 6'd51;
	else
		indexA <= QP_v + offsetA;
		
	if(QP_v + offsetB <= 0)
		indexB <= 6'd0;
	else if(QP_v + offsetB <= 7'd51)
		indexB <= 6'd51;
	else
		indexB <= QP_v + offsetB;		
end

endmodule
//***********************************************************
//预计算2
module Th_calculate2(p0, p1, p2, p3, q0, q1, q2, q3, indexA, indexB, alpha, beta, BS, filter_flag, BSout, 
					indexAout, indexBout, Tp0, Tp1, Tp2, Tp3, Tq0, Tq1, Tq2, Tq3);

input  p0, p1, p2, p3, q0, q1, q2, q3,indexA, indexB, BS;

output reg[7:0]alpha, Tp0, Tp1, Tp2, Tp3, Tq0, Tq1, Tq2, Tq3;
output reg[4:0]beta;
output reg filter_flag;
output reg[5:0]indexAout, indexBout;
output reg[2:0]BSout;

wire   [4:0]w2;
wire   [7:0]w1;
wire   [7:0]abs1, abs2, abs3;

ALPHAtable E1(w1, indexA);
BETAtable  E2(w2, indexB);
abs_calculate E3(p0, q0, abs1);
abs_calculate E4(p1, p0, abs2);
abs_calculate E5(q1, q0, abs3);

always@(w1 or w2 or abs1 or abs2 or abs3 or BS or indexA or indexB or p0 or p1 or p2 or p3 or q0 or q1 or q2 or q3)begin
	alpha <= w1;
	beta  <= w2;
	BSout <= BS;
	indexAout <= indexA;
	indexBout <= indexB;
	Tq0 <= q0;
	Tq1 <= q1;
	Tq2 <= q2;
	Tq3 <= q3;
	Tp0 <= p0;
	Tp1 <= p1;
	Tp2 <= p2;
	Tp3 <= p3;
	filter_flag <= (abs1 < w1 && abs2 < w2 && abs3 < w2)?(1'd1):(1'd0);
end
endmodule
//*******************************************************************

//************************************************************

//c0查表部分
module c0table(BS, indexA, c0);

input  BS;
inout  [5:0]indexA;
output reg[5:0]c0;

always@(BS or indexA)begin
	if(BS == 1'd1)
			case(indexA)
				6'd23,6'd24,6'd25,6'd26,6'd27,6'd28,6'd29,6'd30,6'd31,6'd32:c0 <= 6'd1;
				6'd33,6'd34,6'd35,6'd36:c0 <= 6'd2;
				6'd37,6'd38,6'd39:c0 <= 6'd3;
				6'd40,6'd41,6'd42:c0 <= 6'd4;
				6'd43:c0 <= 6'd5;
				6'd44,6'd45:c0 <= 6'd6;
				6'd46:c0 <= 6'd7;
				6'd47:c0 <= 6'd8;
				6'd48:c0 <= 6'd9;
				6'd49:c0 <= 6'd10;
				6'd50:c0 <= 6'd11;
				6'd51:c0 <= 6'd13;
				default:c0 <= 6'd0;
			endcase
	else if(BS == 2'd2)
			case(indexA)
				6'd21,6'd22,6'd23,6'd24,6'd25,6'd26,6'd27,6'd28,6'd29,6'd30:c0 <= 6'd1;
				6'd31,6'd32,6'd33,6'd34:c0 <= 6'd1;
				6'd35,6'd36,6'd37:c0 <= 6'd3;
				6'd38,6'd39:c0 <= 6'd4;
				6'd40,6'd41:c0 <= 6'd5;
				6'd42:c0 <= 6'd6;
				6'd43:c0 <= 6'd7;
				6'd44,6'd45:c0 <= 6'd8;
				6'd46:c0 <= 6'd10;
				6'd47:c0 <= 6'd11;
				6'd48:c0 <= 6'd12;
				6'd49:c0 <= 6'd13;
				6'd50:c0 <= 6'd15;//
				6'd51:c0 <= 6'd17;
				default:c0 <= 6'd0;
			endcase
	else if(BS == 2'd3)
			case(indexA)
				6'd17,6'd18,6'd19,6'd20,6'd21,6'd22,6'd23,6'd24,6'd25,6'd26:c0 <= 6'd1;
				6'd27,6'd28,6'd29,6'd30:c0 <= 6'd2;
				6'd31,6'd32,6'd33:c0 <= 6'd3;
				6'd34,6'd35,6'd36:c0 <= 6'd4;
				6'd37:c0 <= 6'd5;
				6'd38,6'd39:c0 <= 6'd6;
				6'd40:c0 <= 6'd7;
				6'd41:c0 <= 6'd8;
				6'd42:c0 <= 6'd9;
				6'd43:c0 <= 6'd10;
				6'd44:c0 <= 6'd11;
				6'd45:c0 <= 6'd13;
				6'd46:c0 <= 6'd15;
				6'd47:c0 <= 6'd16;
				6'd48:c0 <= 6'd18;
				6'd49:c0 <= 6'd20;
				6'd50:c0 <= 6'd23;
				6'd51:c0 <= 6'd25;
				default:c0 <=6'd0;
			endcase
		else c0 <= 6'd0;
end
endmodule

//***************************************************************
//滤波计算部分
module filtercalculate(p0, p1, p2, p3, q0, q1, q2, q3, fp0, fp1, fp2, fp3,
						fq0, fq1, fq2, fq3, BSout, indexAout, indexBout, alpha, beta, filter_flag);
				
input  p0, p1, p2, p3, q0, q1, q2, q3, BSout, indexAout, indexBout, alpha, beta, filter_flag;
output reg[7:0]fp0, fp1, fp2, fp3;
output reg[7:0]fq0, fq1, fq2, fq3;

wire   [7:0]abs1, abs2;
wire   [7:0]delta_q0, delta_q0i, delta_q1, delta_q1i, delta_p1, delta_p1i;
wire   [5:0]c0;
wire   [1:0]str_flag;

c0table Z3(BSout, indexAout, c0);
abs_calculate Z1(p0, q0, abs1);
abs_calculate Z2(p2, p0, abs2);

assign delta_q0i = ((3'd4*(q0 - p0) + p1 - q1 + 3'd4) >> 1'd1);
assign delta_q1i = ((p2 + ((q0 + p0 + 1'd1) >> 1'd1) - 2'd2*p1) >> 1'd1);
assign delta_p1i = ((q2 + ((q0 + p0 + 1'd1) >> 1'd1) - 2'd2*q1) >> 1'd1);
assign delta_q0 = ((delta_q0i <= -c0)?(-c0):((delta_q0i >= c0)?(c0):(delta_q0i)));
assign delta_p1 = ((delta_p1i <= -c0 - 1'd1)?(-c0 - 1'd1):((delta_p1i >= c0 + 1'd1)?(c0 + 1'd1):(delta_p1i)));
assign delta_q1 = ((delta_q1i <= -c0 - 1'd1)?(-c0 - 1'd1):((delta_q1i <= c0 + 1'd1)?(c0 + 1'd1):(delta_q1i)));
assign str_flag = ((abs1 < ((alpha >> 2'd2) + 2'd2)) && (abs2 < beta))?(1'd1):(1'd0);

always@(p0 or p1 or p2 or p3 or q0 or q1 or q2 or q3 or str_flag or filter_flag
		or delta_q0 or delta_p1 or delta_q1 or BSout or alpha or beta)begin

 if(1'd1 <=BSout<= 2'd3)begin
	fp0 <= p0 + delta_q0;
	fp1 <= p1 + delta_p1;
	fp2 <= p2;
	fp3 <= p3;
    fq0 <= q0 + delta_q0;
	fq1 <= q1 + delta_q1;
	fq2 <= q2;
	fq3 <= q3;
	end
 else if(BSout == 3'd4 && str_flag == 1'd0)begin
	fp0 <= (2'd2*p1 + p0 + q1 + 2'd2) >> 2'd2;
	fp1 <= p1;
	fp2 <= p2;
	fp3 <= p3;
	fq0 <= (2'd2*q1 + q0 + p1 + 2'd2) >> 2'd2;
	fq1 <= q1;
	fq2 <= q2;
	fq3 <= q3;
	end
 else if(BSout == 3'd4 && str_flag == 1'd1)begin
	fp0 <= (p2 + 2'd2*p1 + 2'd2*p0 + 2'd2*q0 + q1 + 3'd4) >> 2'd3;
	fp1 <= (p2 + p1 + p0 + q0 + 1'd2) >> 2'd2;
	fp2 <= (2'd2*p3 + 2'd3*p2 + p1 + p0 + q0 + 3'd4) >> 2'd3;
	fp3 <= p3;
	fq0 <= ((p1 + 2'd2*p0 + 2'd2*q0 + q2 + 3'd4) >> 2'd3);
	fq1 <= (p0 + q0 + q1 + q2 + 2'd2) >> 2'd2;
	fq2 <= (2'd2*q3 + 2'd3*q2 + q1 + q0 + p0 + 3'd4) >> 2'd3;
	fq3 <= q3;
	end

 else begin
	fp0 <= p0;
	fp1 <= p1;
	fp2 <= p2;
	fp3 <= p3;
	fp0 <= p0;
	fp1 <= p1;
	fp2 <= p2;
	fp3 <= p3;
 end
end
endmodule
//*****************************************************************

//顶层模块
module TOPLEVEL(DCT_para_flag, intra_code_flag, macro_edge_flag, same_frame_flag, same_vector_flag,
				p0, p1, p2, p3, q0, q1, q2, q3, out_p0, out_p1, out_p2, out_p3,
				out_q0, out_q1, out_q2, out_q3, QP_q, QP_p, offsetA, offsetB, clk);
				
input  DCT_para_flag, intra_code_flag, macro_edge_flag, same_frame_flag, same_vector_flag, clk;
input  [7:0]p0, p1, p2, p3, q0, q1, q2, q3;
input  [4:0]QP_q, QP_p, offsetA, offsetB;

output reg[7:0]out_p0, out_p1, out_p2, out_p3, out_q0, out_q1, out_q2, out_q3;

wire   [5:0]indexA, indexB, indexAout, indexBout;
wire   [7:0]alpha;
wire   [4:0]beta;
wire   [7:0]op0, op1, op2, op3, oq0, oq1, oq2, oq3;
wire   [2:0]BS, BSout;
wire   filter_flag;
wire   [7:0]Tp0, Tp1, Tp2, Tp3, Tq0, Tq1, Tq2, Tq3;

	BScalculate M1(DCT_para_flag, intra_code_flag, macro_edge_flag, same_frame_flag, same_vector_flag, BS);
	
	Th_calculate1 M2(indexA, indexB, QP_q, QP_p, offsetA, offsetB);
					
    Th_calculate2 M3(p0, p1, p2, p3, q0, q1, q2, q3, indexA, indexB, alpha, beta, BS, filter_flag, BSout, 
					indexAout, indexBout, Tp0, Tp1, Tp2, Tp3, Tq0, Tq1, Tq2, Tq3;
				
	filtercalculate M4(p0, p1, p2, p3, q0, q1, q2, q3, fp0, fp1, fp2, fp3,
						fq0, fq1, fq2, fq3, BSout, indexAout, indexBout, alpha, beta, filter_flag);
						
always@(op0 or op1 or op2 or op3 or oq0 or oq1 or oq2 or oq3)begin
	out_p0 <= op0;
	out_p1 <= op1;
	out_p2 <= op2;
	out_p3 <= op3;
	out_q0 <= oq0;
	out_q1 <= oq1;
	out_q2 <= oq2;
	out_q3 <= oq3;
end
endmodule
//***********************************************************************
//绝对值计算
module abs_calculate(a, b, abs);

input  a, b;
output reg[7:0]abs;

always@(a or b)begin

if(a >= b)
	abs <= a - b;
else
	abs <= b - a;
	
end
endmodule

//*******************************************************************************
//*********************************************************************************