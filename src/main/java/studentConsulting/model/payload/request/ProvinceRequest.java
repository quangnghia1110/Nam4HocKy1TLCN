package studentConsulting.model.payload.request;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class ProvinceRequest {
    private String name;
    private String nameEn;
    private String fullName;
    private String fullNameEn;
    private String codeName;
}
