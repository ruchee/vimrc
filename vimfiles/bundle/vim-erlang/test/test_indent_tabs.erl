% vi: noexpandtab tabstop=5 shiftwidth=3
%
% This is a test file for indent/erlang.vim that is used to test that the
% indentation works correctly when tabs and spaces are mixed.
%
% Tabstop and shiftwidth are set to peculiar values to make the test more
% interesing.

blocks() ->
   case
	 X
   of
	 A ->
	    begin
		  Expr1,
		  Expr2
	    end;
	 B when
		 Guard1;
		 Guard2A,
		 Guarg2B ->
	    Expr
   end,
   ok.

strings() ->
   begin
	 "a	b	c", begin
				  Expression
			    end,
	 "a
	 b", begin
		   Expression
		end,
   end,
   ok.

atoms() ->
   begin
	 'a	b	c', begin
				  Expression
			    end,
	 'a
	 b', begin
		   Expression
		end,
   end,
   ok.
