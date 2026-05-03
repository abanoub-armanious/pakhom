# Add a `<Code>` XML node to a parent element

Add a `<Code>` XML node to a parent element

## Usage

``` r
.add_code_node(parent, guid, name, is_codable = TRUE, description = NULL)
```

## Arguments

- parent:

  xml2 node to attach to

- guid:

  Character GUID

- name:

  Character code/theme name

- is_codable:

  Logical — TRUE for leaf codes, FALSE for grouping nodes

- description:

  Optional character description

## Value

The newly created xml2 node (invisibly)
