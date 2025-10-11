-record(form_data, {
    values :: list({binary(), binary()}),
    files :: list({binary(), wisp:uploaded_file()})
}).
