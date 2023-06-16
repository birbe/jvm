use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::{parse_macro_input, ItemStruct, Meta};

#[proc_macro_derive(JParse, attributes(prefix))]
pub fn jparse_derive(stream: TokenStream) -> TokenStream {
    let item_struct: ItemStruct = parse_macro_input!(stream);

    let item_struct_ident = &item_struct.ident;

    let mut to_bytes = quote! {};
    let mut from_bytes = quote! {};

    item_struct.fields.iter().for_each(|field| {
        let field_ident = &field.ident;
        let field_type = &field.ty;

        let prefix = field
            .attrs
            .iter()
            .find_map(|attr| match &attr.meta {
                Meta::NameValue(syn::MetaNameValue { path, value, .. }) => {
                    if path.is_ident("prefix") {
                        Some(value.to_token_stream())
                    } else {
                        None
                    }
                }
                _ => None,
            })
            .unwrap_or(quote! { 2 });

        to_bytes.extend(quote! {
            out.extend( self.#field_ident.to_bytes_prefixed::<#prefix>());
        });

        from_bytes.extend(quote! {
            #field_ident: <#field_type>::from_bytes_prefixed::<&mut R, #prefix>(&mut r)?,
        });
    });

    let tk2 = quote! {

        impl jvm_types::JParse for #item_struct_ident {

            type Output = #item_struct_ident;

            fn from_bytes_prefixed<R: std::io::Read, const PREFIX: usize>(mut r: R) -> Result<#item_struct_ident, std::io::Error> {
                Ok(Self { #from_bytes })
            }

            fn to_bytes_prefixed<const PREFIX: usize>(&self) -> Vec<u8> {
                let mut out = Vec::with_capacity(std::mem::size_of::<#item_struct_ident>());

                #to_bytes

                out
            }

        }

    };

    tk2.into()
}
