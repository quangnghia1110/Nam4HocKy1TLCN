package studentConsulting.model.entity.content;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.Column;
import javax.persistence.Embeddable;
import java.io.Serializable;

@Embeddable
@Data
@AllArgsConstructor
@NoArgsConstructor
public class LikeKeyEntity implements Serializable {

    @Column(name = "id_target")
    private Integer targetId;

    @Column(name = "id_user")
    private Integer userId;

    @Column(name = "type")
    private String type;
}