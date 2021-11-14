#include <gtest/gtest.h>
{%- if cookiecutter.binary_type != "exe" %}
#include "{{cookiecutter.project_name}}.hpp"
{%- endif %}

TEST(Test_{{cookiecutter.project_name}}, {{cookiecutter.project_name}}) {
}