# PG types

Experimenting with postgres array and composite types.

**Motivation**
How we model our data is important for understanding it and set some expectation on how it should be used. Here are some examples:
- **Related data**: Fields that are highly correlated can be grouped together. For instance an identifier with a source label
  where it does not make sense to separate. We're often normalize them to multiple columns with a name prefix and this can introduce
  bugs or wrong usage. 
- **Data aggregates**: It does not always make sense to normalize everything down to columns. For instance if we have a
  status field with a status with a timestamp. For experience this is either normalized into multiple columns
  or we add an extra table where the tuple is stored. Common for both of them is that they're cumbersome to use,
  especially when you have this modelled as an array or list in the application layer.
- **Accessibility**: Using the two concepts above we can access our data with one query compared to running multiple
  queries from our application.
- **Discoverability**: Having a more concise model in our database makes it easier to understand how we can access and
  use the data.

**Goal:**
- Parse array and composite results
- Add support for writing and reading array and composite types with doobie and prepared statements


**Current status:**
- [x] Naively parse postgres strings into ast
- [x] Render ast to sql using `row` and `array`
- [x] Render ast to postgres string format
- [ ] Parser: Handle ambiguous postgres strings. (`row('(s)')` and `row(row('s'))`)
- [ ] Parser: Handle empty and single element array values. (unquoted nested arrays)
- [ ] Render ast to postgres string format with correct nested quoting
- [ ] Render: Handle empty and single element array values. (unquoted nested arrays and empty array without type hint)


