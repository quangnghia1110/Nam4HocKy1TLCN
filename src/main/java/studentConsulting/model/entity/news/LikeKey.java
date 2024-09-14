package studentConsulting.model.entity.news;

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
public class LikeKey implements Serializable {

    @Column(name = "id_target", nullable = false)
    private Integer targetId;

    @Column(name = "id_user", nullable = false)
    private Integer userId;
    
    @Column(name = "type", nullable = false)
    private String type;
}