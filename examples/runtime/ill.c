/*
 * examples/runtime/ill.c: performs an illegal instruction
 *
 *
 * Copyright (C) 2020-2022  Rudy Matela
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
int main()
{
	/* https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/tree/include/asm-i386/bug.h?h=v2.6.13 */
	/* https://stackoverflow.com/questions/9314270/how-do-i-force-a-sigill-to-be-sent-to-my-program */
	/* https://docs.microsoft.com/en-us/cpp/intrinsics/ud2?view=msvc-160 */
	/* U2S is an intentionally invalid opcode for IA-32 and IA-64 processors */
	asm("ud2");
	/* running this program forces it to receive a SIGILL signal */
	return 0;
}
