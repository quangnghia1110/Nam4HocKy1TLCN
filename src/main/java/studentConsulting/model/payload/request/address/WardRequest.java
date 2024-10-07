package studentConsulting.model.payload.request.address;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class WardRequest {
    private String name;
    private String nameEn;
    private String fullName;
    private String fullNameEn;
    private String codeName;
}
