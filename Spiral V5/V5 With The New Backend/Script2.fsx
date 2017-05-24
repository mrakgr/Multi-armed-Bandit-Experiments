let d on_rec_with tev d fl fin =
    on_rec_with (fun _ (d,ret) ->
        tev d fl (fun (d,fl) ->
            ret (fl,fun (d,tr) -> fin (d, (tr,fl)))
            )
        ) d
