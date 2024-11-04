package studentConsulting.model.entity;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonManagedReference;
import lombok.*;

import javax.persistence.*;
import java.time.LocalDate;
import java.util.Objects;
import java.util.Set;

@Data
@Builder
@Entity
@Table(name = "department")
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(exclude = {"accounts"})
public class DepartmentEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Integer id;

    @Column(name = "created_at", updatable = false)
    private LocalDate createdAt;

    @Column(name = "name", length = 255, unique = true)
    private String name;

    @Column(name = "description", length = 500)
    private String description;

    @Column(name = "logo", length = 255)
    private String logo;

    @OneToMany(mappedBy = "department", cascade = CascadeType.ALL, orphanRemoval = true)
    @JsonManagedReference
    private Set<AccountEntity> accounts;

    @OneToMany(mappedBy = "department", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<FieldEntity> fields;


    @OneToMany(mappedBy = "department", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<QuestionEntity> questions;

    @OneToMany(mappedBy = "department", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<CommonQuestionEntity> commonQuestions;

    @OneToMany(mappedBy = "fromDepartment", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<ForwardQuestionEntity> fromForwardQuestions;

    @OneToMany(mappedBy = "toDepartment", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<ForwardQuestionEntity> toForwardQuestions;

    @OneToMany(mappedBy = "department", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<ConsultationScheduleEntity> consultationSchedule;

    @OneToMany(mappedBy = "department", cascade = CascadeType.ALL, orphanRemoval = true)
    @JsonIgnore
    private Set<RatingEntity> rating;

    @OneToMany(mappedBy = "department", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<ConversationEntity> conversation;

    @Override
    public int hashCode() {
        return id != null ? id.hashCode() : 0;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        DepartmentEntity that = (DepartmentEntity) o;

        return Objects.equals(id, that.id);
    }
}
