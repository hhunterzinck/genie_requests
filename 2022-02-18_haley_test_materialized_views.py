from synapseclient.table import MaterializedViewSchema
import synapseclient
syn = synapseclient.Synapse()
syn.setEndpoints(**synapseclient.client.STAGING_ENDPOINTS)
syn = synapseclient.login()


temp = MaterializedViewSchema(
    name="my-first-material-view",
    parent="syn25921894",
    definingSQL="SELECT * FROM syn27240986 F JOIN syn27240957 P on (F.patient_id = P.patient_id)"
)
synid_mv = syn.store(temp)
print(synid_mv)
res = syn.tableQuery(f"SELECT * FROM {synid_mv['id']}")
print(list(res))
