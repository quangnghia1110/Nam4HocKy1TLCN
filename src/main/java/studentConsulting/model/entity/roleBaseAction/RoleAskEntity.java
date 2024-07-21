package studentConsulting.model.entity.roleBaseAction;

import javax.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import studentConsulting.model.entity.authentication.RoleEntity;
import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.entity.notification.NotificationEntity;
import studentConsulting.model.entity.questionAnswer.QuestionEntity;

import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Set;

@Data
@Builder
@Entity
@Table(name = "role_ask")
@NoArgsConstructor
@AllArgsConstructor
public class RoleAskEntity {
	@Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id", nullable = false)
    private Integer id; // Mã vai trò người hỏi

    @ManyToOne
    @JoinColumn(name = "role_id", nullable = false, referencedColumnName = "id")
    private RoleEntity role; // Mã vai trò tham chiếu

    @Column(name = "name", nullable = false, length = 50)
    private String name; // Tên vai trò người hỏi

    @Column(name = "created_at", nullable = false)
    private Timestamp createdAt; // Thời gian tạo

    @Column(name = "updated_at", nullable = false, insertable = false, updatable = false, columnDefinition = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP")
    private Timestamp updatedAt; // Thời gian cập nhật
    
    @OneToMany(mappedBy = "roleAsk", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<QuestionEntity> questions;
}
