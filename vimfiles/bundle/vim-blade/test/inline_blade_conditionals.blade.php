@if ($a == 'b') <h1>Same!</h1> @endif
<div>
    <h2 class="big-heading {{{ $h1Class }}} @if ($a == 'b') same-class @endif" @if ($a == 'b') data-extra-attr="same" @endif>
        H2 content
    </h2>
</div>
