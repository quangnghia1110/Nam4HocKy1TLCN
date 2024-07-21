package studentConsulting.model.entity.main;

import javax.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import studentConsulting.model.entity.authentication.UserEntity;

@Data
@Builder
@Entity
@Table(name = "class_users")
@NoArgsConstructor
@AllArgsConstructor
public class ClassUserEntity {

	@Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Long id;

    @Column(name = "class_id", length = 50, nullable = false)
    private String classId;

    @Column(name = "user_id", length = 50, nullable = false)
    private Long userId;

    @Column(name = "status", nullable = false)
    private boolean status = true;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "class_id", referencedColumnName = "id", insertable = false, updatable = false)
    private ClassEntity classEntity;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "user_id", referencedColumnName = "id", insertable = false, updatable = false)
    private UserEntity user;
}
