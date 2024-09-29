package studentConsulting.model.payload.dto.address;

import lombok.AllArgsConstructor;
import lombok.Data;

@Data
@AllArgsConstructor
public class DistrictDTO {
    private String code;
    private String fullName;
}
