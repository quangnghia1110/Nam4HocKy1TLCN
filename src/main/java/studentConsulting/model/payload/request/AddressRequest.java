package studentConsulting.model.payload.request;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class AddressRequest {
    private String line;
    private String provinceCode;
    private String districtCode;
    private String wardCode;
}
