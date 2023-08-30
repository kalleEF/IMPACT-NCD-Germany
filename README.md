# IMPACT NCD Germany microsimulation

--------------------------------------------------------------------------------

IMPACT NCD Germany is an implementation of the IMPACTncd framework, which was developed by Chris
Kypridemos with contributions from Peter Crowther (Melandra Ltd), Maria
Guzman-Castillo, Amandine Robert, and Piotr Bandosz.

Copyright (C) 2018-2023 University of Liverpool, Chris Kypridemos

IMPACT NCD Germany was implemented and adapted to the German context by Karl M.F.
Emmert-Fees with support by Chris Kypridemos. Researchers interested in using the model
for Germany are encouraged to reach out via email to: karl.emmert-fees@tum.de

IMPACTncd is free software; you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the Free Software
Foundation; either version 3 of the License, or (at your option) any later
version. This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
details. You should have received a copy of the GNU General Public License along
with this program; if not, see <http://www.gnu.org/licenses/> or write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

## Current implementation

IMPACT NCD Germany was originally adapted to Germany to evaluate the long-term health
and economic impact of sugar-sweetened beverage taxation in the German population. For this reason
the model currently supports a limited amount of exposures and diseases. However, given
the appropriate data, the model can flexibly expanded to other disease contexts and exposures clusters.

Currently implemented diseases are: Obesity, coronary heart disease, stroke and type 2 diabetes.

Currently implemented exposures are: Sugar-sweetened beverage consumption (inlc. sugar content),
fruit juice consumption (incl. sugar content), body mass index.

Individuals in the model can experience mortality from coronary heart disease, stroke or non-cardiovascular
causes. The model is currently stratified by age and sex, only. However, similarly to the extension
with respect to exposures and diseases, additionally stratification variables can be added relatively easy
given the respective data.

## Further information

- The model is currently mainly based on the German KORA cohort.
- The model currently only compiles under Linux.
- Active development of the C++ component is carried out by Chris Kypridemos at the University of Liverpool.
