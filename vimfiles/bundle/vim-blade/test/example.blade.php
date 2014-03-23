<html>
    <body>
        @section('sidebar')
            This is the master sidebar.
        @stop

        <div class="container">
            @yield('content')
        </div>
    </body>
</html>

@extends('layouts.master')

@section('sidebar')
    @parent

    <p>This is appended to the master sidebar.</p>
@stop

@show

@section('content')
    <p>This is my body content.</p>
@stop

@section('content')
    <p>This is my body content.</p>
@endsection

@overwrite

@lang('message.apples')

@choice('message.apples', 10)

Hello, {{ $name }}.
Hello, {{{ $rawName }}}.

The current UNIX timestamp is {{ time() }}.

{{ $multiline->should()
    ->work()
    ->as()
    ->well()
}}

@{{ This should not be processed by $blade }}
@{{{ Neither should $this }}}

@if (count($records) > 0)
    <p>I have records!</p>
@elseif (count($records) < 0)
    <p>I owe records!</p>
@else
    <p>I want records!</p>
@endif

@unless (Auth::check())
    You are not signed in.
@endunless

@for ($i = 0; $i < 10; $i++)
    <p>The current value is {{ $i }}</p>
@endfor

@foreach ($users as $user)
    <p>This is user {{ $user->id }}</p>
@endforeach

@while (true)
    <p>I'm looping forever.</p>
@endwhile

@include('view.name')

@each('foo', $data, 'bar')

@foreach ($something as $that) @include($that) @endforeach

{{-- This comment will not be in the rendered HTML --}}
