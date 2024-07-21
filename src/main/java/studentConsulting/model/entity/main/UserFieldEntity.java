
package studentConsulting.model.entity.main;

import javax.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import studentConsulting.model.entity.authentication.UserEntity;

import java.io.Serializable;

@Data
@Builder
@Entity
@Table(name = "user_fields")
@NoArgsConstructor
@AllArgsConstructor
public class UserFieldEntity {

	@Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Long id;

    @Column(name = "user_id", length = 50, nullable = false, insertable = false, updatable = false)
    private Long userId;

    @Column(name = "field_id", length = 50, nullable = false, insertable = false, updatable = false)
    private String fieldId;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "user_id", referencedColumnName = "id")
    private UserEntity user;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "field_id", referencedColumnName = "id")
    private FieldEntity field;
}
