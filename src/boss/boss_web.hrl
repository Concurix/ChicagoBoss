
-record(boss_app_info, {
        application,
        base_url,
        static_prefix,
        static_path,
        doc_prefix,
        domains,
        init_data,
        router_config,
        translator_sup_pid,
        translator_pid,
        model_modules = [],
        controller_modules = []
    }).

