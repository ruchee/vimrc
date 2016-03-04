syntax keyword rubyTestMethod
      \ assert
      \ assert_block
      \ assert_empty
      \ assert_equal
      \ assert_in_delta
      \ assert_in_epsilon
      \ assert_includes
      \ assert_instance_of
      \ assert_kind_of
      \ assert_match
      \ assert_nil
      \ assert_operator
      \ assert_predicate
      \ assert_raises
      \ assert_respond_to
      \ assert_same
      \ assert_silent
      \ assert_throws
      \ flunk
      \ must_be
      \ must_be_close_to
      \ must_be_empty
      \ must_be_instance_of
      \ must_be_kind_of
      \ must_be_nil
      \ must_be_same_as
      \ must_be_silent
      \ must_be_within_delta
      \ must_be_within_epsilon
      \ must_equal
      \ must_include
      \ must_match
      \ must_output
      \ must_raise
      \ must_respond_to
      \ must_send
      \ must_throw
      \ pass
      \ refute
      \ refute_empty
      \ refute_equal
      \ refute_in_delta
      \ refute_in_epsilon
      \ refute_includes
      \ refute_instance_of
      \ refute_kind_of
      \ refute_match
      \ refute_nil
      \ refute_operator
      \ refute_predicate
      \ refute_respond_to
      \ refute_same
      \ skip
      \ wont_be
      \ wont_be_close_to
      \ wont_be_empty
      \ wont_be_instance_of
      \ wont_be_kind_of
      \ wont_be_nil
      \ wont_be_same_as
      \ wont_be_within_delta
      \ wont_be_within_epsilon
      \ wont_equal
      \ wont_include
      \ wont_match
      \ wont_respond_to

syntax keyword rubyTestStatement
      \ after
      \ before
      \ context
      \ expect
      \ setup
      \ should
      \ teardown
      \ scenario
      \ feature
      \ background

" See after/syntax/ruby/minitest.vim for "describe, it" blocks definition

highlight link rubyTestMethod Function
highlight link rubyTestStatement Statement
