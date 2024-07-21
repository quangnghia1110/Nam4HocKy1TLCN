package studentConsulting.model.entity.departmentField;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import studentConsulting.model.entity.questionAnswer.CommonQuestionEntity;
import studentConsulting.model.entity.questionAnswer.QuestionEntity;

import java.sql.Timestamp;
import java.util.Set;

@Data
@Builder
@Entity
@Table(name = "fields")
@NoArgsConstructor
@AllArgsConstructor
public class FieldEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id", nullable = false)
    private Integer id; // Mã lĩnh vực

    @Column(name = "name", nullable = false, length = 255)
    private String name; // Tên lĩnh vực

    @ManyToOne
    @JoinColumn(name = "department_id", nullable = false, referencedColumnName = "id")
    private DepartmentEntity department; // Mã phòng ban tham chiếu

    @Column(name = "created_at", nullable = false, updatable = false)
    private Timestamp createdAt; // Thời gian tạo

    @Column(name = "updated_at", nullable = false)
    private Timestamp updatedAt; // Thời gian cập nhật
    
    @OneToMany(mappedBy = "field", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<QuestionEntity> questions;
    
    @OneToMany(mappedBy = "field", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<CommonQuestionEntity> commonQuestions;
}
