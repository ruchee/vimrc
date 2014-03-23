<p>
    {{ $address }}{{-- Double braces on purpose: already escaped --}}
    @if ($address && $document->getMyText('phone'))
        <br/>
    @endif
    @if ($document->getMyText('phone'))
        T: <a href="tel:{{{ $document->getMyText('phone') }}}">Phone {{{ $document->getMyText('phone') }}}</a>
    @endif
    @if ($document->getMyText('phone') && $document->getMyText('email'))
        or
    @endif
    @if ($document->getMyText('email'))
        E: <a href="mailto:{{{ $document->getMyText('email') }}}">Email {{{ $document->getMyText('email') }}}</a>
    @endif
</p>
<em>
    {{ $address }}{{-- Double braces on purpose: already escaped --}}
    @if ($address && $document->getMyText('phone'))
        <br/>
    @endif
    @if ($document->getMyText('phone'))
        T: <a href="tel:{{{ $document->getMyText('phone') }}}">Phone {{{ $document->getMyText('phone') }}}</a>
    @endif
    @if ($document->getMyText('phone') && $document->getMyText('email'))
        or
    @endif
    @if ($document->getMyText('email'))
        E: <a href="mailto:{{{ $document->getMyText('email') }}}">Email {{{ $document->getMyText('email') }}}</a>
    @endif
</em>
<strong>
    {{ $address }}{{-- Double braces on purpose: already escaped --}}
    @if ($address && $document->getMyText('phone'))
        <br/>
    @endif
    @if ($document->getMyText('phone'))
        T: <a href="tel:{{{ $document->getMyText('phone') }}}">Phone {{{ $document->getMyText('phone') }}}</a>
    @endif
    @if ($document->getMyText('phone') && $document->getMyText('email'))
        or
    @endif
    @if ($document->getMyText('email'))
        E: <a href="mailto:{{{ $document->getMyText('email') }}}">Email {{{ $document->getMyText('email') }}}</a>
    @endif
</strong>
<a href="http://example.com">
    {{ $address }}{{-- Double braces on purpose: already escaped --}}
    @if ($address && $document->getMyText('phone'))
        <br/>
    @endif
    @if ($document->getMyText('phone'))
        T: <span class="tel:{{{ $document->getMyText('phone') }}}">Phone {{{ $document->getMyText('phone') }}}</span>
    @endif
    @if ($document->getMyText('phone') && $document->getMyText('email'))
        or
    @endif
    @if ($document->getMyText('email'))
        E: <span class="mailto:{{{ $document->getMyText('email') }}}">Email {{{ $document->getMyText('email') }}}</span>
    @endif
</a>
