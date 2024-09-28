package studentConsulting.model.entity.roleBaseAction;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import studentConsulting.model.entity.authentication.RoleEntity;
import studentConsulting.model.entity.questionAnswer.QuestionEntity;

import javax.persistence.*;
import java.time.LocalDate;
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
    @Column(name = "id")
    private Integer id;

    @Column(name = "created_at", updatable = false)
    private LocalDate createdAt;

    @ManyToOne
    @JoinColumn(name = "role_id", referencedColumnName = "id")
    private RoleEntity role;

    @Column(name = "name", length = 50)
    private String name;

    @OneToMany(mappedBy = "roleAsk", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<QuestionEntity> questions;
}
