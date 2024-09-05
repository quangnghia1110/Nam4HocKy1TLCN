package studentConsulting.model.payload.dto;

import lombok.AllArgsConstructor;
import lombok.Data;

@Data
@AllArgsConstructor
public class WardDTO {
    private String code;
    private String fullName;
}
