package dev.ikm.tinkar.starterdata;

import dev.ikm.tinkar.terms.EntityProxy;

import java.util.UUID;

public class StarterDataTerm {

    //TODO-aks8m: These Patterns below need to be in Tinkar Bindings
    public static final EntityProxy.Pattern IDENTIFIER_PATTERN = EntityProxy.Pattern.make("Identifier Pattern", UUID.fromString("5d60e14b-c410-5172-9559-3c4253278ae2"));
    public static final EntityProxy.Pattern AXIOM_SYNTAX_PATTERN = EntityProxy.Pattern.make("Axiom Syntax Pattern", UUID.fromString("c0ca180b-aae2-5fa1-9ab7-4a24f2dfe16b"));
    public static final EntityProxy.Pattern PATH_MEMBERSHIP_PATTERN = EntityProxy.Pattern.make("Path membership", UUID.fromString("1eb187f9-9844-55de-b575-9487ea0b2c61"));
    public static final EntityProxy.Pattern VERSION_CONTROL_PATTERN = EntityProxy.Pattern.make("Version Control Pattern", UUID.fromString("cad7b375-e4c0-5fd8-a346-63b8718f74a3"));
    public static final EntityProxy.Pattern AUTHORING_BASE_MODEL_PATTERN = EntityProxy.Pattern.make("Authoring Base Model Pattern", UUID.fromString("6070f6f5-893d-5144-adce-7d305c391cf9"));
    public static final EntityProxy.Pattern TINKAR_CORE_BASE_MODEL_PATTERN = EntityProxy.Pattern.make("Tinkar Core Base Model Pattern", UUID.fromString("b8669480-0527-4a85-b4fc-ea7445faa517"));
    public static final EntityProxy.Pattern KOMET_BASE_MODEL_PATTERN = EntityProxy.Pattern.make("Komet Base Model Pattern", UUID.fromString("bbbbf1fe-00f0-55e0-a19c-6300dbaab9b2"));

    public static final EntityProxy.Concept STATED_NAVIGATION =
            EntityProxy.Concept.make("Stated navigation (SOLOR)", UUID.fromString("614017af-9903-53d9-aab4-15fd02193dce"));
    public static final EntityProxy.Concept INFERRED_NAVIGATION =
            EntityProxy.Concept.make("Inferred navigation (SOLOR)", UUID.fromString("4bc6c333-7fc9-52f1-942d-f8decba19dc2"));

    public static final EntityProxy.Pattern STATED_DEFINITION_PATTERN = EntityProxy.Pattern.make("Stated definition pattern", UUID.fromString("e813eb92-7d07-5035-8d43-e81249f5b36e"));
    public static final EntityProxy.Pattern INFERRED_DEFINITION_PATTERN = EntityProxy.Pattern.make("Inferred definition pattern", UUID.fromString("9f011812-15c9-5b1b-85f8-bb262bc1b2a2"));

}