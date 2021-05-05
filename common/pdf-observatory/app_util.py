
import json


def config_load(config_path):
    """Load config and munge in pipeline plugins (like parsers) into the core
    config.
    """
    cfg = json.load(open(config_path))

    # Live config munging -- merge down pipeline views (decision and file
    # detail). This is to minimize developer investment, since they're really
    # the same as top-level views.
    for mergeable in ['parsers', 'file_detail_views', 'decision_views']:
        for pipe_name, pipe_cfg in cfg['pipelines'].items():
            if pipe_cfg.get('disabled', False):
                # Don't transfer any of this pipeline's plugins
                continue

            pipe_label = pipe_cfg.get('label', pipe_name)
            for view_name, view_cfg in pipe_cfg[mergeable].items():
                node_cfg = view_cfg.copy()
                node_cfg['pipeline'] = pipe_name
                if 'label' in node_cfg:
                    node_cfg['label'] = f'{pipe_label} -- {node_cfg["label"]}'
                cfg[mergeable][pipe_name + '--' + view_name] = node_cfg

    return cfg

