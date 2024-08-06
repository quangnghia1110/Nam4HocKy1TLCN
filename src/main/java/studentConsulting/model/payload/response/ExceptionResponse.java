package studentConsulting.model.payload.response;

import javax.persistence.Entity;


import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;


@Data
@Builder
@Entity
@NoArgsConstructor
@AllArgsConstructor
public class ExceptionResponse {
	private int status;
    private String message;
}
