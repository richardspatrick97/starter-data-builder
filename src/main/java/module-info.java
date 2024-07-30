open module tinkar.sandbox {
    requires dev.ikm.tinkar.terms;
    requires dev.ikm.tinkar.schema;
    requires dev.ikm.tinkar.provider.ephemeral;
    requires dev.ikm.tinkar.provider.spinedarray;
    requires dev.ikm.tinkar.provider.entity;
    requires dev.ikm.tinkar.provider.executor;
    requires dev.ikm.tinkar.provider.search;
    requires dev.ikm.tinkar.coordinate;
    requires transitive dev.ikm.tinkar.bindings;
    requires transitive dev.ikm.tinkar.transformer.api;
    requires transitive com.google.protobuf;
    requires org.eclipse.collections.api;
    requires java.logging;
    requires com.fasterxml.jackson.databind;
    requires org.slf4j;
    requires org.eclipse.collections;


    exports dev.ikm.tinkar.starterdata;

}