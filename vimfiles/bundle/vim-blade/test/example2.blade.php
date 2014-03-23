{{-- <ul class="errors">
    @foreach($errors->get('first_name') as $message)
        <li>{{ $message }}</li>
    @endforeach
</ul> --}}

{{ Form::label('first_name', 'First Name', array('id' => 'first_name')) }}
{{ Form::text('first_name', 'chan') }}
{{ Form::submit('Save') }}
{{ Form::close() }}



{{-- 
 <ul class="errors">
    @foreach($errors->get('first_name') as $message)
        <li>{{ $message }}</li>
    @endforeach
</ul> 
--}}

 {{ Form::label('first_name', 'First Name', array('id' => 'first_name')) }}
 {{ Form::text('first_name', 'chan') }}
 {{ Form::submit('Save') }}
 {{ Form::close() }}



{{-- Important to have no leading whitespace
--}}<li data-node-id="{{{ $model->getKey() }}}">
    <div class="sort-tolerance">
        <span class="sort-handle glyphicon glyphicon-move"></span>
        {{{ $model->name }}}
    </div>
    @include('admin.menu-items.children', ['model' => $model])
</li>{{--
This comment stops any trailing whitespace
--}}
