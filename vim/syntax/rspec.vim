"
" specky: syntax highlighting for rspec files.
" $Id: rspec.vim 70 2009-06-01 14:33:54Z mahlon $
"

runtime! syntax/ruby.vim
unlet b:current_syntax

syntax keyword rspecGroupMethods describe it
highlight link rspecGroupMethods Type

syntax keyword rspecBeforeAndAfter after after_suite_parts append_after append_before before before_suite_parts prepend_after prepend_before
highlight link rspecBeforeAndAfter Statement

syntax keyword rspecMocks mock stub
highlight link rspecMocks Constant

syntax keyword rspecMockMethods and_raise and_return and_throw and_yield build_child called_max_times expected_args invoke matches
highlight link rspecMockMethods Function

syntax keyword rspecKeywords should should_not should_not_receive should_receive
highlight link rspecKeywords Constant

syntax keyword rspecMatchers be_a be_a_kind_of be_an be_an_instance_of be_close be_false be_instance_of be_kind_of be_nil be_true change eql equal exist expect have have_at_least have_at_most have_exactly include match matcher raise_error respond_to satisfy throw_symbol to to_not wrap_expectation
highlight link rspecMatchers Function

syntax keyword rspecMessageExpectation advise any_number_of_times at_least at_most exactly expected_messages_received generate_error ignoring_args matches_at_least_count matches_at_most_count matches_exact_count matches_name_but_not_args negative_expectation_for never once ordered similar_messages times twice verify_messages_received with
highlight link rspecMessageExpectation Function

let b:current_syntax = "rspec"
