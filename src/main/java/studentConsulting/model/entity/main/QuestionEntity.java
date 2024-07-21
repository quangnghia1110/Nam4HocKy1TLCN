package studentConsulting.model.entity.main;

import javax.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import studentConsulting.model.entity.authentication.UserEntity;

import java.time.LocalDateTime;
import java.util.List;

@Data
@Builder
@Entity
@Table(name = "questions")
@NoArgsConstructor
@AllArgsConstructor
public class QuestionEntity {

    @Id
    @Column(name = "id", length = 50, nullable = false)
    private Integer id;

    @Column(name = "title", length = 255, nullable = false)
    private String title;

    @Column(name = "content", columnDefinition = "TEXT", nullable = false)
    private String content;

    @Column(name = "date", nullable = false, columnDefinition = "DATETIME DEFAULT CURRENT_TIMESTAMP")
    private LocalDateTime date;

    @Column(name = "status", nullable = false)
    private int status = 0;

    @Column(name = "views", nullable = false)
    private int views = 0;

    @Column(name = "user_id", length = 50, nullable = false)
    private Long userId;

    @Column(name = "department_id", length = 50, nullable = false)
    private String departmentId;

    @Column(name = "field_id", length = 50, nullable = false)
    private String fieldId;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "user_id", insertable = false, updatable = false)
    private UserEntity user;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "department_id", insertable = false, updatable = false)
    private DepartmentEntity department;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "field_id", insertable = false, updatable = false)
    private FieldEntity field;

    @OneToMany(mappedBy = "question", fetch = FetchType.LAZY)
    private List<AnswerEntity> answers;
}
