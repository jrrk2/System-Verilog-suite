#!/usr/bin/env python3
"""
Cross-check Verilator JSON structure to understand why
std_icache shows 0 signals/processes after conversion
"""

import json
import sys

def analyze_module(module_data, module_name):
    """Analyze a single module from Verilator JSON"""

    print(f"\n{'='*60}")
    print(f"Module: {module_name}")
    print('='*60)

    stmts = module_data.get('stmtsp', [])
    print(f"Total statements: {len(stmts)}")

    # Count statement types
    stmt_types = {}
    vars = []
    always_blocks = []

    for stmt in stmts:
        stmt_type = stmt.get('type', 'unknown')
        stmt_types[stmt_type] = stmt_types.get(stmt_type, 0) + 1

        if stmt_type == 'VAR':
            vars.append(stmt)
        elif stmt_type == 'ALWAYS':
            always_blocks.append(stmt)

    print(f"\nStatement types:")
    for stype, count in sorted(stmt_types.items(), key=lambda x: -x[1]):
        print(f"  {stype:20s}: {count:3d}")

    print(f"\nVAR declarations: {len(vars)}")
    if vars:
        print("  First 10 variables:")
        for i, var in enumerate(vars[:10]):
            print(f"    {i+1:2d}. {var.get('name', 'unnamed')}")

    # Count _q variables
    q_vars = [v for v in vars if '_q' in v.get('name', '').lower()]
    print(f"\n  Variables ending in _q: {len(q_vars)}")
    for var in q_vars:
        print(f"    - {var.get('name')}")

    print(f"\nALWAYS blocks: {len(always_blocks)}")
    for i, always in enumerate(always_blocks):
        keyword = always.get('keyword', 'always')
        senses = always.get('sensesp', [])
        body_stmts = always.get('stmtsp', [])
        print(f"  {i+1}. {keyword} (sensitivity: {len(senses)}, body: {len(body_stmts)} stmts)")

    # Check if statements are at the top level or nested
    nested_stmts = []
    for stmt in stmts:
        if 'stmtsp' in stmt:
            nested_stmts.append(stmt.get('type', 'unknown'))

    if nested_stmts:
        print(f"\nStatements with nested stmtsp: {len(nested_stmts)}")
        print(f"  Types: {set(nested_stmts)}")

def main():
    if len(sys.argv) < 2:
        print("Usage: ./verify_module_extraction.py <verilator_json>")
        sys.exit(1)

    json_file = sys.argv[1]

    print(f"Loading: {json_file}")
    with open(json_file) as f:
        data = json.load(f)

    modules = data.get('modulesp', [])
    print(f"\nTotal modules in JSON: {len(modules)}")

    # Find std_icache
    std_icache = None
    for module in modules:
        if module.get('name') == 'std_icache':
            std_icache = module
            break

    if std_icache:
        analyze_module(std_icache, 'std_icache')
    else:
        print("\n✗ std_icache not found in JSON")
        print("\nModules containing 'cache':")
        for module in modules:
            name = module.get('name', '')
            if 'cache' in name.lower():
                print(f"  - {name}")

    # Also check how many modules have VAR and ALWAYS at top level
    print("\n" + "="*60)
    print("Module structure summary:")
    print("="*60)

    modules_with_vars = 0
    modules_with_always = 0
    modules_empty = 0

    for module in modules:
        stmts = module.get('stmtsp', [])
        has_var = any(s.get('type') == 'VAR' for s in stmts)
        has_always = any(s.get('type') == 'ALWAYS' for s in stmts)

        if has_var:
            modules_with_vars += 1
        if has_always:
            modules_with_always += 1
        if not stmts:
            modules_empty += 1

    print(f"Modules with VAR declarations:   {modules_with_vars}/{len(modules)}")
    print(f"Modules with ALWAYS blocks:      {modules_with_always}/{len(modules)}")
    print(f"Modules with empty stmtsp:       {modules_empty}/{len(modules)}")

if __name__ == '__main__':
    main()
