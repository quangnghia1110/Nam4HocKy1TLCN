package studentConsulting.model.entity;

import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.*;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

@Entity
@Table(name = "comment")
@Data
@AllArgsConstructor
@NoArgsConstructor
public class CommentEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id_comment")
    private Integer idComment;

    @ManyToOne(fetch = FetchType.EAGER)
    @JoinColumn(name = "id_post")
    private PostEntity post;

    @ManyToOne(fetch = FetchType.EAGER)
    @JoinColumn(name = "id_user_comment")
    @JsonIgnore
    private UserInformationEntity userComment;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "id_comment_father")
    private CommentEntity parentComment;

    @Column(nullable = false)
    private String comment;

    @Column(nullable = false)
    private LocalDate createDate;

    @OneToMany(mappedBy = "parentComment", cascade = CascadeType.ALL, orphanRemoval = true)
    @JsonIgnore
    private List<CommentEntity> childComments = new ArrayList<>();

    public CommentEntity(Integer idComment) {
        this.idComment = idComment;
    }

    @Override
    public int hashCode() {
        return idComment != null ? idComment.hashCode() : 0;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        CommentEntity that = (CommentEntity) o;

        return Objects.equals(idComment, that.idComment);
    }
}

