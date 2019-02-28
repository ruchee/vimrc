<?php if($foo='bar' ) { $something() } ?>
Hello, {{ $name }}.

The current UNIX timestamp is {{ time() }}.

Hello, @{{ name }}.

{{ isset($name) ? $name : 'Default' }}

Hello, {!! $name !!}.

@if ($foo == 'bar') @endif

@if (count($records) === 1)
    I have one record!
@elseif (count($records) > 1)
    I have multiple records!
@else
    I don't have any records!
@endif

@unless (Auth::check())
    You are not signed in.
@endunless

@for ($i = 0; $i < 10; $i++)
    The current value is {{ $i }}
@endfor

@foreach ($users as $user)
    <p>This is user {{ $user->id }}</p>
    @break
    @continue
@endforeach

@forelse ($users as $user)
    <li>{{ $user->name }}</li>
@empty
    <p>No users</p>
@endforelse

@while (true)
    <p>I'm looping forever.</p>
@endwhile

@can('update-post', $post)
    <!-- current user can update the post -->
@elsecan('delete-post', $post)
    <!-- current user can delete the post -->
@else
    <!-- current user can't update or delete the post -->
@endcan

@cannot('view-post')
    <!-- current user cannot view the post -->
@elsecannot('publish-post')
    <!-- current user cannot publish the post -->
@endcannot

<div>
    @include('shared.errors')
    <form>
        <!-- Form Contents -->
    </form>
</div>

@each('view.name', $jobs, 'job')

{{-- This comment will not be present in the rendered HTML --}}

{{--
    This comment spans
    multiple lines
    @yield('dont highlight')
--}}

{{-- todo fixme xxx note TODO FIXME XXX NOTE --}}

@inject('metrics', 'App\Services\MetricsService')

@push('scripts')
    <script src="/example.js"></script>
@endpush

<head>
    <!-- Head Contents -->
    <title>
        @hasSection('title')
            Test - @yield('title')
        @else
            Test
        @endif
    </title>

    @stack('scripts')
</head>

@prepend('scripts')
    <script src="{{ mix('/js/manifest.js') }}"></script>
    <script src="{{ mix('/js/vendor.js') }}"></script>
@endprepend

<div>
    @section('sidebar')
        This is the master sidebar.
    @show

    @yield('content')
</div>

@section('title', 'Page Title')

@section('sidebar')
    @parent
    <p>This is appended to the master sidebar.</p>
@endsection

<input name="example" {{ old('example') ? 'checked' : '' }} />

<?php
    $collection = collect([
        'foo' => [
            'bar',
            'baz',
        ]
    ])
?>

@include('pages.home', [
    'foo' => [
        'bar',
        'baz',
    ]
])

{{
    sprintf(
        'This %s a multiline echo statement',
        $foo
    )
}}

@cache
    A custom Blade directive
    @datetime($var)
    @namespaced::directive($var)
@endcache

@php($var = 'Hello World')
@unset($var)

@php
    $environment = isset($env) ? $env : 'testing';
@endphp

do_not_highlight@php.net

@verbatim
    <p class="highlighted">@if(true) {{ $notHighlighted }} @endif</p>
    <!-- highlighted -->
    <?php /* also highlighted */ ?>
@endverbatim

@lang('messages.welcome')
@choice('messages.items', 3)

@component('app')
    @slot('title')
        Title goes here
    @endslot
@endcomponent

@json($foo)

@isset($foo)
    records
@endisset

@empty($foo)
    records
@endempty

@auth('admin')
    authenticated
@endauth

@guest('admin')
    not authenticated
@endguest

@switch($i)
    @case(1)
        case code
        @break
@endswitch

@includeFirst
