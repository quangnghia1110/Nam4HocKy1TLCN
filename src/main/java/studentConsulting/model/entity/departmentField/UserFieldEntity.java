package studentConsulting.model.entity.departmentField;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import studentConsulting.model.entity.authentication.UserInformationEntity;

import java.sql.Timestamp;

@Data
@Builder
@Entity
@Table(name = "user_fields")
@NoArgsConstructor
@AllArgsConstructor
public class UserFieldEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id", nullable = false)
    private Integer id; // Mã user field

    @ManyToOne
    @JoinColumn(name = "user_id", nullable = false, referencedColumnName = "id")
    private UserInformationEntity user; // Mã người dùng tham chiếu

    @ManyToOne
    @JoinColumn(name = "field_id", nullable = false, referencedColumnName = "id")
    private FieldEntity field; // Mã lĩnh vực tham chiếu

    @Column(name = "created_at", nullable = false, updatable = false)
    private Timestamp createdAt; // Thời gian tạo

    @Column(name = "updated_at", nullable = false)
    private Timestamp updatedAt; // Thời gian cập nhật
}

