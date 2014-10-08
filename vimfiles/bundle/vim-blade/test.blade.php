/* Regular PHP */
echo('foo')         // shouldn't be highlighted
<?php if ($bar->foo == $foo) return false; ?>

/* Regular HTML */
<html>
</html>

/* Echos */
{!! $name !!}
{{ $name }}
{{{ $name }}}
@{!! $name !!}      // should be treated as regular php

/* Structures */
@else
@empty
@endfor
@endforeach
@endif
@endpush
@endsection
@endunless
@endwhile
@overwrite
@show
@stop

/* Structures (with conditions) */
@append('foo')
@choice('language.line', 1)
@each(['foo', 'bar'])
@if ($bar == ($foo*2))
    {{ $bar }}
@elseif ($bar == ($foo*3))
    {{ $bar * 2 }}
@else
    'foo'
@endif
@extends('layouts.default')
@for($i = 0; $i < 10; $i++)
    the value is {{ $i }}
@endforeach
@forelse($users as $user)
    <li>{{ $user->name }}</li>
@empty
    <p>No users</p>
@endforelse
@include('something')
@lang('language.line')
@push('foo')
    <h1>{{ $foo }}</h1>
@endpush
@section('nav')
    <h1>Home</h1>
@endsection
@stack('content')
@unless(1 == 2)
    {{ $foo }}
@endunless
@while(true == false)
    {{ $foo }}
@endwhile
@yield('content')

/* Comments */
{{-- comments on one line --}}
{{-- comments
     on 
     many
     lines --}}
{{-- structures shouldn't escape @if($insideComments == true) --}}
@yield('section') {{-- comments after structure are valid --}}
{{ $comments->finished() }}

/* Edge Cases */
// try a multiline include
@include('admin.misc.sort-header', [
    'column' => 'name',
])

// inside html
<div>
    @if ($foo == $bar)
        {{ $foo }}
    @endif
</div>

// keywords inside of comments
{{--
List from http://www.php.net/manual/en/reserved.keywords.php
__halt_compiler()
abstract
and
array()
as
break
callable
case
catch
class
clone
const
continue
declare
default
die()
do
echo
else
elseif
empty()
enddeclare
endfor
endforeach
endif
endswitch
endwhile
eval()
exit()
extends
final
finally
for
foreach
function
global
goto
if
implements
include
include_once
instanceof
insteadof
interface
isset()
list()
namespace
new
or
print
private
protected
public
require
require_once
return
static
switch
throw
trait
try
unset()
use
var
while
xor
yield
__DIR__
__FILE__
__FUNCTION__
__LINE__
__METHOD__
__NAMESPACE__
__TRAIT__
--}
