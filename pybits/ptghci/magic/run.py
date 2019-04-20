

def run(session, dispatcher, cmd, args):
    filename = args.strip()
    try:
        with open(filename) as f:
            lines = f.readlines()
    except FileNotFoundError:
        return {'success': False,
                'content': 'File %s not found' % args}

    n_execs = 0
    cur = []
    result_lines = []
    
    # Split lines into groups.  Start a new group when the first char on a line
    # is not a space.
    cur_group = []
    groups = [cur_group]
    for (i, l) in enumerate(lines):
        if len(l) == 0:
            cur_group.append('')
            continue
        if l[0].isspace():
            cur_group.append(l)
        else:
            cur_group = [l]
            groups.append(cur_group)

    for g in groups:
        # Start a new group
        result_lines.extend(['> '+c for c  in g])
        to_exec = '\n'.join(g)
        resp = dispatcher.dispatch(to_exec)
        if resp['success']:
            result_lines.extend(['< '+c 
                                for c in resp['content'].splitlines()])
        else:
            result_lines.append("Error: ")
            result_lines.extend(resp['content'])
            break
        n_execs += 1

    return {'success': True,
            'content': (('Executed %d commands,'
                        ' assignments and top-level declarations\n')%n_execs)
                        + '\n'.join(result_lines)}

